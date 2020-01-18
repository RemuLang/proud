from proud.core_lang import scope, composable_evaluator as ce, types
from proud.core_lang.modular_compiler import *


def make_quote(cgc: CompilerGlobalContext, Express):
    tenv = cgc.tenv
    tc_state = cgc.tc_state

    class Quote(Express, ce.Eval_unquote):

        def unquote(module, contents):
            return Express(module.outer).eval(contents)

        def __init__(self, clc: CompilerLocalContext):
            self.outer = clc
            inner_scope = self.inner_scope = scope.Scope(True, None)
            inner_clc = clc.with_scope(inner_scope)
            Express.__init__(self, inner_clc)

        def eval_sym(self, x: str):
            try:
                return Express.eval_sym(self, x)
            except scope.Undef:
                var = scope.Sym(x, object(), False)
                self.inner_scope.freevars[x] = var
                tenv[var] = tc_state.new_var(Var=types.Var, loc=self.clc.location, filename=self.clc.filename, name=x)
                return Express.eval_sym(self, x)

    return Quote
