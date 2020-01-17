from proud.core_lang.modular_compiler import CompilerGlobalContext, CompilerLocalContext
from proud.core_lang.lowered_to_sexpr import SExprGen, resolve_type
from proud.core_lang.modular_compiler.setup import make_compiler_evaluators
from proud.core_lang.scope import Sym, Scope
from proud.parser.parser_wrap import parse
from dataclasses import dataclass
import typing


@dataclass
class ModuleFinder:
    exist: typing.Optional[Sym]
    filename: str
    get_code: typing.Callable[[], str]


class BackEnd:
    filename: str

    def codegen(self, ctx: CompilerGlobalContext, sexpr: tuple) -> str:
        raise NotImplementedError

    def init_global_context(self) -> CompilerGlobalContext:
        raise NotImplementedError

    def mk_top_scope(self) -> Scope:
        raise NotImplementedError

    def search_module(self, qualname: typing.List[str]) -> ModuleFinder:
        """
        Given a qualified name of a module, return the filename, and a thunk which can produce the source code
        """
        raise NotImplementedError

    def remember_module(self, path, mod_sym: Sym) -> None:
        raise NotImplementedError

    def main(self, in_file, out_file):
        self.filename = in_file
        ctx = self.init_global_context()
        Modular, *_ = make_compiler_evaluators(ctx)
        with open(in_file) as f:
            src = f.read()
        ast = parse(src, in_file)
        clc = CompilerLocalContext(scope=self.mk_top_scope(), filename=in_file, path='')
        mod_expr = Modular(clc).eval(ast)
        resolve_type(mod_expr, ctx)
        sexpr = SExprGen().eval(mod_expr)
        code = self.codegen(ctx, sexpr)
        with open(out_file, 'w') as f:
            f.write(code)
