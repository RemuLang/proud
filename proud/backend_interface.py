import sys

from proud import excs
from proud.core_lang.modular_compiler import CompilerGlobalContext, CompilerLocalContext
from proud.core_lang.lowered_to_sexpr import SExprGen, resolve_type
from proud.core_lang.modular_compiler.setup import make_compiler_evaluators
from proud.core_lang.modular_compiler import Location
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
    ctx: CompilerGlobalContext
    types_to_show: typing.List[typing.Tuple[Location, str, Sym]]
    verbose: bool

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
        if not hasattr(self, 'verbose'):
            self.verbose = False
        self.filename = in_file
        self.types_to_show = []
        self.ctx = ctx = self.init_global_context()
        Modular, *_ = make_compiler_evaluators(ctx)
        with open(in_file) as f:
            src = f.read()
        ast = parse(src, in_file)
        clc = CompilerLocalContext(scope=self.mk_top_scope(), filename=in_file, path='')
        try:
            mod_expr = Modular(clc).eval(ast)
        except excs.StaticCheckingFailed as e:
            clc = typing.cast(CompilerLocalContext, e.clc)
            if self.verbose:
                raise
            else:
                sys.exit('{} {} at file {}, line {}, column {}'.format(e.exc.__class__.__name__, e.exc, clc.filename,
                                                                       clc.location[0], clc.location[1]))

        resolve_type(mod_expr, ctx)
        sexpr = SExprGen().eval(mod_expr)
        code = self.codegen(ctx, sexpr)

        tenv = ctx.tenv
        tc_state = ctx.tc_state
        for loc, filename, each in self.types_to_show:
            print('[file: {} |line: {} |column: {}]\n'.format(filename, loc[0], loc[1]), each.name, ':',
                  tc_state.infer(tenv[each]))

        with open(out_file, 'w') as f:
            f.write(code)
