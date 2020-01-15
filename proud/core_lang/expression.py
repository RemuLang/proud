from proud import composable_evaluator as ce
from proud import excs, sexpr
from proud import lowered_ir as ir
from proud import types
from proud.core_lang import *
from proud.scope import Scope
from hybridts import type_encoding as te
import typing

try:
    # noinspection PyUnresolvedReferences
    from proud.core_lang.typing import Typing
    from proud.core_lang.quotation import Quote
except ImportError:
    pass


class Express(Evaluator, ce.Eval_let, ce.Eval_lam, ce.Eval_match,
              ce.Eval_annotate, ce.Eval_binary, ce.Eval_list, ce.Eval_tuple,
              ce.Eval_record, ce.Eval_call, ce.Eval_attr, ce.Eval_quote,
              ce.Eval_loc, ce.Eval_coerce, ce.Eval_literal, ce.Eval_type):
    def type(module, name, definition):
        assert name is None
        t = Typing(module.comp_ctx).eval(definition)
        return ir.Expr(expr=ir.Const(t), type=te.App(types.type_type, t))

    def literal(module, val):
        my_type = type_map[type(val)]
        return ir.Expr(type=my_type, expr=ir.Const(val))

    def coerce(module, expr):
        expr = module.eval(expr)
        loc, _ = sexpr.unloc(expr)
        return ir.Expr(expr=ir.Coerce(expr),
                       type=types.Var(loc,
                                      module.comp_ctx.filename,
                                      name='coerce_var'))

    def quote(module, contents):
        quotation = Quote(module.comp_ctx)
        quote_expr = quotation.eval(contents)
        scope: Scope = quotation.comp_ctx.scope
        arg_sym = scope.enter(".external")
        name_var = types.Var(module._loc,
                             module.comp_ctx.filename,
                             name=".external")
        tenv = module.comp_ctx.tenv
        tenv[arg_sym] = name_var
        record_fields = []

        for attr_name, each in scope.freevars.items():
            record_fields.append((each, attr_name, tenv[each]))
        rho_group = types.ForallScope(module._loc,
                                      filename=module.comp_ctx.filename)
        rho = te.Fresh('ρ', rho_group)
        arg_type = te.Forall(
            rho_group, (rho, ),
            te.Record(
                te.row_from_list([(attr, t) for _, attr, t in record_fields],
                                 te.RowPoly(rho))))
        module.comp_ctx.tc_state.unify(name_var, arg_type)
        arg_expr = ir.Expr(type=arg_type, expr=arg_sym)
        block = [
            ignore(
                ir.Set(
                    each,
                    ir.Expr(type=t, expr=ir.Field(base=arg_expr, attr=attr))))
            for each, attr, t in record_fields
        ]
        block.append(quote_expr)
        ret_expr = ir.Fun("<quote>", module.comp_ctx.filename, [arg_sym],
                          ir.Expr(expr=ir.Block(block), type=quote_expr.type))
        ret_type = te.Arrow(arg_type, quote_expr.type)
        return ir.Expr(expr=ret_expr, type=ret_type)

    def attr(module, base, attr_name: str):
        base = module.eval(base)
        var = types.Var(loc=module._loc,
                        filename=module.comp_ctx.filename,
                        name=attr_name + ".getter")
        tho = types.Var(loc=module._loc,
                        filename=module.comp_ctx.filename,
                        name=attr_name + ".tho")
        record_type = te.Record(
            te.row_from_list([(attr_name, var)], te.RowPoly(tho)))
        module.comp_ctx.tc_state.unify(record_type, base.type)
        return ir.Expr(expr=ir.Field(base=base, attr=attr_name), type=var)

    def record(module, pairs, row):
        kv = []
        for each in pairs:
            loc, (attr_name, expr) = sexpr.unloc(each)
            expr = module.eval(expr)
            kv.append((attr_name, expr))

        kv.sort(key=lambda tp: tp[0])
        elts = [elt for _, elt in kv]
        mono = te.Record(
            te.row_from_list([(k, v.type) for k, v in kv], te.empty_row))
        left = ir.Expr(expr=ir.Tuple(elts), type=mono)

        if not row:
            return left

        right = module.eval(row)

        my_type = te.Record(
            te.row_from_list([(k, v.type) for k, v in kv],
                             te.RowPoly(right.type)))

        return ir.Expr(expr=ir.Merge(left=left, right=right), type=my_type)

    def tuple(module, elts):
        if not elts:
            return ir.Expr(expr=ir.Const(()), type=types.unit_t)
        if len(elts) is 1:
            return module.eval(elts[0])

        elts = list(map(module.eval, elts))
        return ir.Expr(expr=ir.Tuple(elts),
                       type=te.Tuple(tuple(e.type for e in elts)))

    def list(module, elts):
        raise NotImplementedError

    def binary(module, head, tl: typing.Tuple[tuple, ...]):
        raise NotImplementedError

    def annotate(module, var, type):
        var = module.eval(var)
        type = Typing(module.comp_ctx).eval(type)
        module.comp_ctx.tc_state.unify(var.type, type)
        return var

    def match(module, target, cases: list):
        raise NotImplementedError

    def loc(module, location, contents):
        module._loc = location
        a = module.eval(contents)
        return ir.Expr(type=a.type, expr=ir.WrapLoc(location, a))

    def call(module, f, arg):
        loc, f = sexpr.unloc(f)
        tc_state = module.comp_ctx.tc_state
        f = module.eval(f)
        arg = module.eval(arg)
        f.type = tc_state.infer(f.type)
        arg.type = tc_state.infer(arg.type)

        _, f_type = tc_state.inst_without_structure_preserved(f.type)
        f_type = tc_state.infer(f_type)

        f_inst = None
        if isinstance(f_type, te.Implicit):
            f_inst = f_type.witness
            f_type = f_type.type
            assert not isinstance(f_type, te.Implicit)
        if isinstance(arg.type, te.Forall):
            arg_map, arg_type = tc_state.inst_without_structure_preserved(
                arg.type)
        else:
            arg_map, arg_type = {}, arg.type

        arg_inst = None
        if isinstance(arg_type, te.Implicit):
            arg_inst = arg_type.witness
            arg_type = arg_type.type
            assert not isinstance(arg_type, te.Implicit)

        ret_t = types.Var(module._loc, module.comp_ctx.filename, name="ret")

        inst_arrow = te.Arrow(arg_type, ret_t)
        tc_state.unify(inst_arrow, f_type)
        a = tc_state.infer(inst_arrow)
        b = tc_state.infer(f_type)
        if arg_map:
            gen_bounds = {}
            forall_scope = types.ForallScope(module._loc,
                                             module.comp_ctx.filename)
            for fresh_or_var, path_type in arg_map.items():
                if not isinstance(fresh_or_var, te.Fresh):
                    continue
                v = tc_state.infer(path_type)
                if not isinstance(v, te.Var):
                    continue

                if not gen_bounds.get(v):
                    gen_bounds[v] = te.Fresh(fresh_or_var.name, forall_scope)
            if gen_bounds:
                bounds = tuple(gen_bounds.values())

                ret_t = tc_state.infer(ret_t)
                arg_type = tc_state.infer(arg_type)

                def _subst(_, t: te.T):
                    v = gen_bounds.get(t, t)
                    if v:
                        return (), v
                    if isinstance(t, te.Var):
                        v = gen_bounds[t] = te.InternalVar(t.is_rigid)
                        return (), v
                    return (), t

                ret_t = te.Forall(forall_scope, bounds,
                                  te.pre_visit(_subst)((), ret_t))

                arg_type = te.Forall(forall_scope, bounds,
                                     te.pre_visit(_subst)((), arg_type))

        if f_inst:
            f = ir.Expr(type=f_type,
                        expr=ir.Instance(f_inst, module.comp_ctx.scope, f))
        else:
            f.type = f_type

        if arg_inst:
            arg = ir.Expr(type=arg_type,
                          expr=ir.Instance(arg_inst, module.comp_ctx.scope,
                                           arg))
        else:
            arg.type = arg_type

        return ir.Expr(type=ret_t, expr=ir.Invoke(f, arg))

    def lam(module, arg, type, ret):
        loc, name = sexpr.unloc(arg)
        prev_comp_ctx = module.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv
        sub_scope = prev_comp_ctx.scope.sub_scope(hold_bound=False)
        filename = prev_comp_ctx.filename
        path = prev_comp_ctx.path
        with keep(module):
            arg_e = sub_scope.enter(name)
            module.comp_ctx = CompilerCtx(sub_scope, tc_state, tenv, filename,
                                          path)
            if type:
                arg_t = tc_state.infer(Typing(module.comp_ctx).eval(type))
            else:
                arg_t = types.Var(loc, filename, name=name + '\'t')
            tenv[arg_e] = arg_t

            ret = module.eval(ret)
            my_type = te.Arrow(arg_t, ret.type)

            freevars = list(sub_scope.freevars.values())
            name = "{} |{}|".format(path, name)
            me = ir.Fun(name, filename, arg_e, ret)
            return ir.Expr(type=my_type, expr=me)

    def let(module, is_rec, name, type, bound, body):
        loc, name = sexpr.unloc(name)
        prev_comp_ctx = module.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv
        sub_scope = module.comp_ctx.scope.sub_scope(hold_bound=False)
        with keep(module):
            module.comp_ctx = CompilerCtx(sub_scope, tc_state, tenv,
                                          prev_comp_ctx.filename,
                                          prev_comp_ctx.path)
            if type:
                my_type_for_unify = my_type = tc_state.infer(
                    Typing(module.comp_ctx).eval(type))
                if isinstance(my_type, te.Forall):
                    my_type_for_unify = types.remove_bound_scope(my_type)
            else:
                my_type = my_type_for_unify = types.Var(
                    loc, module.comp_ctx.filename, name)

            if is_rec:
                me = sub_scope.enter(name)
                tenv[me] = my_type
                bound = module.eval(bound)
            else:
                bound = module.eval(bound)
                me = sub_scope.enter(name)
                tenv[me] = my_type

            bound_type = tc_state.infer(bound.type)

            if type:
                _, bound_type = tc_state.inst_without_structure_preserved(
                    bound_type)
                bound.type = bound_type

            tc_state.unify(my_type_for_unify, bound_type)
            body = module.eval(body)

            my_exp = ir.Block(
                [ignore(ir.Loc(loc)),
                 ignore(ir.Set(me, bound)), body])
            return ir.Expr(type=my_type, expr=my_exp)

    def eval_sym(self, x: str) -> ir.Expr:
        my_type = self.comp_ctx.type_of_value(self.comp_ctx.scope.require(x))
        return ir.Expr(type=my_type, expr=x)

    def eval(self, x) -> ir.Expr:
        if sexpr.is_ast(x):
            hd, *args = x
            return ce.dispatcher[hd](*args)(self)

        if isinstance(x, str):
            return self.eval_sym(x)

        raise TypeError(x)
