from proud import excs
from proud.core_lang import sexpr, composable_evaluator as ce, types
from proud.core_lang.modular_compiler import *
from proud.core_lang.scope import Sym
import proud.unification.type_encode as te
import typing


def make(cgc: CompilerGlobalContext):
    tc_state = cgc.tc_state
    tenv = cgc.tenv

    Express = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)

    def link_express(evaluators):
        nonlocal Express
        Express = evaluators['Express']

    def type_of_type(sym: Sym) -> te.T:
        t = tc_state.infer(tenv[sym])
        if isinstance(t, te.App) and t.f is types.type_type:
            return t.arg
        what_i_want = tc_state.new_var()
        tc_state.unify(t, te.App(types.type_type, what_i_want))
        return what_i_want

    class Typing(Interpreter, ce.Eval_forall, ce.Eval_exist, ce.Eval_arrow, ce.Eval_imply, ce.Eval_tuple,
                 ce.Eval_record, ce.Eval_list, ce.Eval_loc, ce.Eval_call, ce.Eval_type):

        def type(module, name, definition):
            assert name is None
            clc = module.clc
            x = Express(clc).eval(definition).type
            ret = tc_state.new_var(Var=types.Var, loc=clc.location, filename=clc.filename, name="valtotype")
            tc_state.unify(te.App(types.type_type, ret), x)
            ret = tc_state.infer(ret)
            return ret

        def call(module, f, arg):
            f = module.eval(f)
            arg = module.eval(arg)
            return te.App(f, arg)

        def record(module, pairs, row):
            _, pairs = sexpr.unloc(pairs)
            if row:
                row_t = te.RowPoly(module.eval(row))
            else:
                row_t = te.empty_row

            fields = [(k, module.eval(v)) for k, v in pairs]
            row_t = te.row_from_list(fields, row_t)
            return te.Record(row_t)

        def list(module, elts):
            list_t = types.list_t
            if not elts:
                return list_t

            try:
                [a] = elts
                list_elt_type = module.eval(a)
                return te.App(list_t, list_elt_type)
            except ValueError:
                raise excs.InvalidListType("List of {}".format(elts))

        def tuple(module, elts):
            if not elts:
                return types.unit_t

            if len(elts) is 1:
                return module.eval(elts[0])

            return te.Tuple(tuple(map(module.eval, elts)))

        def imply(module, arg, ret):
            arg = module.eval(arg)
            ret = module.eval(ret)
            return te.Implicit(arg, ret)

        def arrow(module, arg, ret):
            arg = module.eval(arg)
            ret = module.eval(ret)
            return te.Arrow(arg, ret)

        def forall(module, unbound_fresh_decls: typing.Tuple[str, ...], polytype):
            fresh_vars_ = set(unbound_fresh_decls)
            if len(fresh_vars_) != len(unbound_fresh_decls):
                raise excs.DuplicatedNamedTypeVar(fresh_vars_)
            unbound_fresh_decls = fresh_vars_
            clc = module.clc
            forall_scope = types.ForallScope(clc.location, clc.filename)
            bound_vars = []
            with clc.resume_scope():
                clc.scope = sub_scope = clc.scope.sub_scope()
                for n in unbound_fresh_decls:
                    _, n = sexpr.unloc(n)
                    sym = sub_scope.enter(n)
                    bound_var = te.Bound(n, forall_scope)
                    bound_vars.append(bound_var)
                    tenv[sym] = te.App(types.type_type, bound_var)
                return te.Forall(forall_scope, tuple(bound_vars), module.eval(polytype))

        def exist(module, bound_vars: tuple, monotype):
            bound_vars_ = set(bound_vars)
            if len(bound_vars) != len(bound_vars_):
                raise excs.DuplicatedNamedTypeVar(bound_vars_)
            bound_vars = bound_vars_
            filename = module.clc.filename
            scope = module.clc.scope
            for n in bound_vars:
                loc, n = sexpr.unloc(n)
                sym = scope.enter(n)
                tvar = tc_state.user_var(Var=types.Var, loc=loc, filename=filename, name=n)
                tenv[sym] = te.App(types.type_type, tvar)

            return module.eval(monotype)

        def eval(self, x):
            try:
                if isinstance(x, str):
                    return type_of_type(self.clc.scope.require(x))
                if sexpr.is_ast(x):
                    hd, *args = x
                    if hd is sexpr.attr_k:
                        return self.type(None, x)
                    return ce.dispatcher[hd](*args)(self)

                raise ValueError(x)
            except excs.StaticCheckingFailed:
                raise
            except Exception as e:
                raise excs.StaticCheckingFailed(e, self.clc)

        def loc(module, location, contents):
            module.clc.location = location
            return module.eval(contents)

    return Typing, [link_express]
