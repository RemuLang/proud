from proud import composable_evaluator as ce
from proud import excs, sexpr
from proud import types
from proud.core_lang import *
from hybridts import type_encoding as te
import typing

try:
    # noinspection PyUnresolvedReferences
    from proud.core_lang.expression import Express
except ImportError:
    pass


class Typing(Evaluator, ce.Eval_forall, ce.Eval_exist, ce.Eval_arrow,
             ce.Eval_imply, ce.Eval_tuple, ce.Eval_record, ce.Eval_list,
             ce.Eval_loc, ce.Eval_call, ce.Eval_type):
    def type(module, name, definition):
        assert name is None
        x = Express(module.comp_ctx).eval(definition).type
        ret = types.Var(module._loc,
                        module.comp_ctx.filename,
                        name="valtotype")

        module.comp_ctx.tc_state.unify(te.App(types.type_type, ret), x)
        ret = module.comp_ctx.tc_state.infer(ret)
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

        forall_scope = types.ForallScope(module._loc, module.comp_ctx.filename)
        fresh_vars = []
        for n in unbound_fresh_decls:
            _, n = sexpr.unloc(n)
            var = module.comp_ctx.scope.enter(n)
            fresh_var = te.Fresh(n, forall_scope)
            fresh_vars.append(fresh_var)
            module.comp_ctx.tenv[var] = te.App(types.type_type, fresh_var)

        return te.Forall(forall_scope, tuple(fresh_vars),
                         module.eval(polytype))

    def exist(module, bound_vars: tuple, monotype):
        bound_vars_ = set(bound_vars)
        if len(bound_vars) != len(bound_vars_):
            raise excs.DuplicatedNamedTypeVar(bound_vars_)
        bound_vars = bound_vars_
        tenv = module.comp_ctx.tenv
        filename = module.comp_ctx.filename
        for n in bound_vars:
            loc, n = sexpr.unloc(n)
            var = module.comp_ctx.scope.enter(n)
            tvar = types.Var(loc, filename)
            tenv[var] = te.App(types.type_type, tvar)

        return module.eval(monotype)

    def eval(self, x):
        if isinstance(x, str):
            return self.comp_ctx.type_of_type(self.comp_ctx.scope.require(x))
        if sexpr.is_ast(x):
            hd, *args = x
            return ce.dispatcher[hd](*args)(self)

        raise ValueError(x)

    def loc(module, location, contents):
        module._loc = location
        return module.eval(contents)
