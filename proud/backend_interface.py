from proud.core_lang.modular_compiler import Evaluator, CompilerCtx
from proud.core_lang.lowered_to_sexpr import SExprGen
from proud.core_lang.scope import Sym
from proud.core_lang.lowered_ir import Expr
from proud.parser.parser_wrap import parse
from dataclasses import dataclass
import typing


@dataclass
class ModuleFinder:
    exist: typing.Optional[Sym]
    filename: str
    get_code: typing.Callable[[], str]


class BackEnd:
    def codegen(self, compiler_ctx: CompilerCtx, sexpr: tuple) -> str:
        raise NotImplementedError

    def init_compiler_ctx(self, filename, path) -> CompilerCtx:
        raise NotImplementedError

    def search_module(self, qualname: typing.List[str]) -> ModuleFinder:
        """
        Given a qualified name of a module, return the filename, and a thunk which can produce the source code
        """
        raise NotImplementedError

    def remember_module(self, path, mod_sym: Sym) -> None:
        raise NotImplementedError

    def main(self, in_file, out_file):
        from proud.core_lang.modular_compiler.module import Modular
        ctx = self.init_compiler_ctx(in_file, path='Main')
        with open(in_file) as f:
            src = f.read()
        ast = parse(src, in_file)
        mod_expr = Modular(ctx).eval(ast)
        sexpr = SExprGen().eval(mod_expr)
        code = self.codegen(ctx, sexpr)
        with open(out_file, 'w') as f:
            f.write(code)
