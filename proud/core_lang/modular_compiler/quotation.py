from proud.core_lang.modular_compiler.expression import Express
from proud.core_lang import scope, composable_evaluator as ce, types
from proud.core_lang.modular_compiler import *


class Quote(Express, ce.Eval_unquote):
    def unquote(module, contents):
        return Express(module.outer).eval(contents)

    def __init__(self, comp_ctx: CompilerCtx):
        tc_state = comp_ctx.tc_state
        tenv = comp_ctx.tenv
        filename = comp_ctx.filename
        path = comp_ctx.path

        self.outer = CompilerCtx(comp_ctx.scope.sub_scope(), tc_state, tenv,
                                 filename, path)
        inner_scope = self.inner_scope = scope.Scope(True, None)
        comp_ctx = CompilerCtx(inner_scope, tc_state, tenv, filename, path)
        Express.__init__(self, comp_ctx)

    def eval_sym(self, x: str):
        try:
            return Express.eval_sym(self, x)
        except scope.Undef:
            var = scope.Sym(x, object(), False)
            self.inner_scope.freevars[x] = var
            self.comp_ctx.tenv[var] = types.Var(
                loc=self._loc, filename=self.comp_ctx.filename, name=x)
            return Express.eval_sym(self, x)
