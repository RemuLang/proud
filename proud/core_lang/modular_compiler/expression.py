from proud.core_lang import sexpr, lowered_ir as ir, composable_evaluator as ce, types
from proud.core_lang.scope import Scope, Sym
from proud.core_lang.polymorphisms import implicit_and_generalise, implicit_and_generalise_, resolve_instance_
from hybridts import type_encoding as te
from proud.core_lang.modular_compiler import *
import typing

try:
    # noinspection PyUnresolvedReferences
    from proud.core_lang.modular_compiler.typing import Typing
    # noinspection PyUnresolvedReferences
    from proud.core_lang.modular_compiler.quotation import Quote
except ImportError:
    pass


def make(cgc: CompilerGlobalContext):
    tc_state = cgc.tc_state
    backend = cgc.backend
    tenv = cgc.tenv

    Typing = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)
    Quote = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)

    def link_typing(**evaluators):
        nonlocal Typing
        Typing = evaluators['Typing']

    def link_quote(**evaluators):
        nonlocal Quote
        Quote = evaluators['Quote']

    def type_of_value(sym: Sym):
        return tenv[sym]

    def type_of_type(sym: Sym) -> te.T:
        t = tc_state.infer(tenv[sym])
        if isinstance(t, te.App) and t.f is types.type_type:
            return t.arg
        what_i_want = te.InternalVar(is_rigid=False)
        tc_state.unify(t, te.App(types.type_type, what_i_want))
        return what_i_want

    def value_of_type(sym: Sym):
        return tenv[sym]

    class Express(Interpreter, ce.Eval_let, ce.Eval_lam, ce.Eval_match, ce.Eval_annotate,
                  ce.Eval_binary, ce.Eval_list, ce.Eval_tuple, ce.Eval_record,
                  ce.Eval_call, ce.Eval_attr, ce.Eval_quote, ce.Eval_loc, ce.Eval_coerce,
                  ce.Eval_literal, ce.Eval_type, ce.Eval_extern, ce.Eval_ite):

        def eval_with_polymorphisms_(self, x):
            clc = self.clc
            x: ir.Expr = self.eval(x)
            implicit_and_generalise_(clc.scope,
                                     tc_state,
                                     x,
                                     loc=clc.location,
                                     filename=clc.filename)
            return x

        def eval_with_polymorphisms(self, x):
            clc = self.clc
            x: ir.Expr = self.eval(x)
            after_unification = implicit_and_generalise(clc.scope,
                                                        tc_state,
                                                        x,
                                                        loc=clc.location,
                                                        filename=clc.filename)
            return x, after_unification

        def eval_expr_with_polymorphisms(self, x: ir.Expr):
            clc = self.clc
            after_unification = implicit_and_generalise(clc.scope,
                                                        tc_state,
                                                        x,
                                                        loc=clc.location,
                                                        filename=clc.filename)
            return x, after_unification

        def extern(module, foreign_code):
            clc = module.clc
            return ir.Expr(expr=ir.Extern(foreign_code),
                           type=types.Var(clc.location, clc.filename, name="extern\'"))

        def ite(module, cond, true_clause, else_clause):

            cond, after_unif1 = module.eval_with_polymorphisms(cond)
            tc_state.unify(cond.type, types.bool_t)
            after_unif1()

            tc, after_unif2 = module.eval_with_polymorphisms(true_clause)
            ec, after_unif3 = module.eval_with_polymorphisms(else_clause)

            tc_state.unify(tc.type, ec.type)
            after_unif2()
            after_unif2()

            return ir.Expr(expr=ir.ITE(cond, tc, ec), type=tc.type)

        def type(module, name, definition):
            assert name is None
            t = Typing(module.clc).eval(definition)
            return ir.Expr(expr=ir.Const(t), type=te.App(types.type_type, t))

        def literal(module, val):
            my_type = type_map[type(val)]
            return ir.Expr(type=my_type, expr=ir.Const(val))

        def coerce(module, expr):
            expr = module.eval(expr)
            loc, _ = sexpr.unloc(expr)
            return ir.Expr(expr=ir.Coerce(expr),
                           type=types.Var(loc, module.clc.filename, name='coerce_var'))

        def attr(module, base, attr_name: str):
            base, after_base_unif = module.eval_with_polymorphisms(base)
            loc, filename = module.clc.location, module.clc.filename
            var = types.Var(loc=loc, filename=filename, name=attr_name + ".getter")
            tho = types.Var(loc=loc, filename=filename, name=attr_name + ".tho")
            record_type = te.Record(te.row_from_list([(attr_name, var)], te.RowPoly(tho)))

            # TODO: how to deal with a polymorphic record value?
            # e.g., forall a. {x : a}
            tc_state.unify(record_type, base.type)
            after_base_unif()

            return ir.Expr(expr=ir.Field(base=base, attr=attr_name), type=var)

        def record(module, pairs, row):
            kv = []

            for each in pairs:
                loc, (attr_name, expr) = sexpr.unloc(each)
                expr = module.eval_with_polymorphisms_(expr)
                kv.append((attr_name, expr))

            kv.sort(key=lambda tp: tp[0])
            elts = [elt for _, elt in kv]
            mono = te.Record(te.row_from_list([(k, v.type) for k, v in kv], te.empty_row))
            left = ir.Tuple(elts)

            if not row:
                left = ir.Expr(expr=ir.Tuple([anyway(left),
                                              anyway(ir.Tuple([]))]),
                               type=mono)
                return left

            right = module.eval_with_polymorphisms_(row)
            my_type = te.Record(
                te.row_from_list([(k, v.type) for k, v in kv], te.RowPoly(right.type)))
            return ir.Expr(expr=ir.Merge(left=anyway(left), right=right), type=my_type)

        def tuple(module, elts):
            if not elts:
                return ir.Expr(expr=ir.Const(()), type=types.unit_t)
            if len(elts) is 1:
                return module.eval_with_polymorphisms_(elts[0])

            elts = list(map(module.eval_with_polymorphisms_, elts))
            return ir.Expr(expr=ir.Tuple(elts),
                           type=te.Tuple(tuple(e.type for e in elts)))

        def annotate(module, var, type):
            var, after_unif = module.eval_with_polymorphisms(var)
            type = Typing(module.clc).eval(type)
            tc_state.unify(var.type, type)
            after_unif()
            return var

        def loc(module, location, contents):
            module.clc.location = location
            a = module.eval(contents)
            return ir.Expr(type=a.type, expr=ir.WrapLoc(location, a))

        def call(module, f, arg):
            clc = module.clc

            exp_f = module.eval(f)
            to_mono = None
            to_poly = None
            func_type = exp_f.type

            # row polymorphisms
            while isinstance(func_type, te.Implicit):
                func_type = func_type.type
            if isinstance(func_type, te.Arrow):
                # layout of record might change correspondingly.
                to_mono = func_type.ret
                to_poly = func_type.arg

            _, f.type = tc_state.inst_without_structure_preserved(f.type, rigid=True)
            resolve_instance_(clc.scope, f)

            arg, after_arg_unif = module.eval_with_polymorphisms(arg)
            ret_t = types.Var(loc=clc.location, filename=clc.filename, name="ret")

            inst_arrow = te.Arrow(arg.type, ret_t)
            tc_state.unify(inst_arrow, f.type)

            ret_t, = after_arg_unif(ret_t, )

            if to_poly:
                arg = ir.Expr(type=arg.type,
                              expr=ir.Polymorphization(layout_type=to_poly, expr=arg))

            ret = ir.Expr(type=ret_t, expr=ir.Invoke(f, arg))

            if to_mono:
                ret = ir.Expr(type=ret_t,
                              expr=ir.Momomorphization(layout_type=to_mono, expr=ret))
            return ret

        def lam(module, arg, type, ret):
            """
            fun x -> x
            """
            loc, name = sexpr.unloc(arg)
            clc = module.clc
            scope = clc.scope
            sub_scope = scope.sub_scope(hold_bound=True)
            filename = clc.filename
            path = clc.path
            with clc.resume_scope():
                clc.scope = sub_scope
                sym_arg = sub_scope.enter(name)
                if type:
                    type_arg = tc_state.infer(Typing(clc).eval(type))
                else:
                    type_arg = types.Var(loc, filename, name=name + '\'t')

                tenv[sym_arg] = type_arg
                ret = module.eval(ret)
                type_arg = tc_state.infer(type_arg)
                arrow_type = te.Arrow(type_arg, ret.type)
                name = "{} |{}|".format(path, name)
                sym_arg = ir.Fun(name, filename, sym_arg, ret)
                return ir.Expr(type=arrow_type, expr=sym_arg)


class Express(Evaluator, ce.Eval_let, ce.Eval_lam, ce.Eval_match, ce.Eval_annotate,
              ce.Eval_binary, ce.Eval_list, ce.Eval_tuple, ce.Eval_record, ce.Eval_call,
              ce.Eval_attr, ce.Eval_quote, ce.Eval_loc, ce.Eval_coerce, ce.Eval_literal,
              ce.Eval_type, ce.Eval_extern, ce.Eval_ite):

    def extern(module, foreign_code):
        return ir.Expr(expr=ir.Extern(foreign_code),
                       type=types.Var(module._loc,
                                      module.comp_ctx.filename,
                                      name="extern\'"))

    def ite(module, cond, true_clause, else_clause):
        cond = module.eval(cond)
        tc_state = module.comp_ctx.tc_state
        tc_state.unify(cond.type, types.bool_t)
        true_clause = module.eval(true_clause)
        else_clause = module.eval(else_clause)
        tc_state.unify(true_clause.type, else_clause.type)
        return ir.Expr(expr=ir.ITE(cond, true_clause, else_clause), type=true_clause.type)

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
                       type=types.Var(loc, module.comp_ctx.filename, name='coerce_var'))

    def attr(module, base, attr_name: str):
        base = module.eval(base)
        tc_state = module.comp_ctx.tc_state
        var = types.Var(loc=module._loc,
                        filename=module.comp_ctx.filename,
                        name=attr_name + ".getter")
        tho = types.Var(loc=module._loc,
                        filename=module.comp_ctx.filename,
                        name=attr_name + ".tho")
        record_type = te.Record(te.row_from_list([(attr_name, var)], te.RowPoly(tho)))

        # TODO: how to deal with a polymorphic record value?
        # e.g., forall a. {x : a}
        tc_state.unify(record_type,
                       tc_state.inst_without_structure_preserved(base.type)[1])
        return ir.Expr(expr=ir.Field(base=base, attr=attr_name), type=var)

    def record(module, pairs, row):
        kv = []
        for each in pairs:
            loc, (attr_name, expr) = sexpr.unloc(each)
            expr = module.eval(expr)
            kv.append((attr_name, expr))

        kv.sort(key=lambda tp: tp[0])
        elts = [elt for _, elt in kv]
        mono = te.Record(te.row_from_list([(k, v.type) for k, v in kv], te.empty_row))
        left = ir.Tuple(elts)
        left = ir.Expr(expr=ir.Tuple([anyway(left), anyway(ir.Tuple([]))]), type=mono)

        if not row:
            return left

        right = module.eval(row)

        my_type = te.Record(
            te.row_from_list([(k, v.type) for k, v in kv], te.RowPoly(right.type)))

        return ir.Expr(expr=ir.Merge(left=left, right=right), type=my_type)

    def tuple(module, elts):
        if not elts:
            return ir.Expr(expr=ir.Const(()), type=types.unit_t)
        if len(elts) is 1:
            return module.eval(elts[0])

        elts = list(map(module.eval, elts))
        return ir.Expr(expr=ir.Tuple(elts), type=te.Tuple(tuple(e.type for e in elts)))

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

        to_mono = None
        to_poly = None
        if isinstance(f.type, te.Forall) and isinstance(f.type.poly_type, te.Arrow):
            # layout of record might change correspondingly.
            func = f.type.poly_type
            to_mono = func.ret
            to_poly = func.arg

        _, f_type = tc_state.inst_without_structure_preserved(f.type)
        f_type = tc_state.infer(f_type)

        f_inst = None
        if isinstance(f_type, te.Implicit):
            f_inst = f_type.witness
            f_type = f_type.type
            assert not isinstance(f_type, te.Implicit)

        if isinstance(arg.type, te.Forall):
            arg_map, arg_type = tc_state.inst_without_structure_preserved(arg.type)
        else:
            arg_map, arg_type = {}, arg.type

        arg_inst = None
        if isinstance(arg_type, te.Implicit):
            arg_inst = arg_type.witness
            arg_type = arg_type.type
            assert not isinstance(arg_type, te.Implicit)

        morph_arg_t = arg_type
        morph_ret_t = ret_t = types.Var(module._loc, module.comp_ctx.filename, name="ret")

        inst_arrow = te.Arrow(arg_type, ret_t)
        tc_state.unify(inst_arrow, f_type)

        if arg_map:
            gen_bounds = {}
            forall_scope = types.ForallScope(module._loc, module.comp_ctx.filename)
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

                morph_ret_t = ret_t
                morph_arg_t = arg_type
                ret_t = te.Forall(forall_scope, bounds, te.pre_visit(_subst)((), ret_t))
                arg_type = te.Forall(forall_scope, bounds,
                                     te.pre_visit(_subst)((), arg_type))

        f.type, f_type = f_type, f.type
        if f_inst:
            f = ir.Expr(type=f_type, expr=ir.Instance(f_inst, module.comp_ctx.scope, f))
        else:
            f.type = f_type

        if arg_inst:
            arg = ir.Expr(type=arg_type,
                          expr=ir.Instance(arg_inst, module.comp_ctx.scope, arg))
        else:
            arg.type = arg_type

        if to_poly:
            arg.type = morph_arg_t
            arg = ir.Expr(type=arg_type,
                          expr=ir.Polymorphization(layout_type=to_poly, expr=arg))

        ret = ir.Expr(type=ret_t, expr=ir.Invoke(f, arg))
        if to_mono:
            ret.type = morph_ret_t
            ret = ir.Expr(type=ret_t,
                          expr=ir.Momomorphization(layout_type=to_mono, expr=ret))
        return ret

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
            module.comp_ctx = prev_comp_ctx.with_scope(sub_scope)
            if type:
                arg_t = tc_state.infer(Typing(module.comp_ctx).eval(type))
            else:
                arg_t = types.Var(loc, filename, name=name + '\'t')
            tenv[arg_e] = arg_t

            ret = module.eval(ret)
            my_type = te.Arrow(arg_t, ret.type)

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
            module.comp_ctx = prev_comp_ctx.with_scope(sub_scope)
            if type:
                my_type_for_unify = my_type = tc_state.infer(
                    Typing(module.comp_ctx).eval(type))
                if isinstance(my_type, te.Forall):
                    my_type_for_unify = types.remove_bound_scope(my_type)
            else:
                my_type = my_type_for_unify = types.Var(loc, module.comp_ctx.filename,
                                                        name)

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
                _, bound_type = tc_state.inst_without_structure_preserved(bound_type)
                bound.type = bound_type

            tc_state.unify(my_type_for_unify, bound_type)
            body = module.eval(body)

            my_exp = ir.Block([ignore(ir.WrapLoc(loc, ignore(ir.Set(me, bound)))), body])
            return ir.Expr(type=body.type, expr=my_exp)

    def eval_sym(self, x: str) -> ir.Expr:
        my_type = self.comp_ctx.type_of_value(self.comp_ctx.scope.require(x))
        var = self.comp_ctx.scope.require(x)
        return ir.Expr(type=my_type, expr=var)

    def eval(self, x) -> ir.Expr:
        if sexpr.is_ast(x):
            hd, *args = x
            return ce.dispatcher[hd](*args)(self)

        if isinstance(x, str):
            return self.eval_sym(x)

        raise TypeError(x)

    def quote(module, contents):
        quotation = Quote(module.comp_ctx)
        quote_expr = quotation.eval(contents)
        scope: Scope = quotation.comp_ctx.scope
        arg_sym = scope.enter(".external")
        name_var = types.Var(module._loc, module.comp_ctx.filename, name=".external")
        tenv = module.comp_ctx.tenv
        tenv[arg_sym] = name_var
        record_fields = []

        for attr_name, each in scope.freevars.items():
            record_fields.append((each, attr_name, tenv[each]))
        rho_group = types.ForallScope(module._loc, filename=module.comp_ctx.filename)
        rho = te.Fresh('ρ', rho_group)
        arg_type = te.Record(
            te.row_from_list([(attr, t) for _, attr, t in record_fields],
                             te.RowPoly(rho)))
        module.comp_ctx.tc_state.unify(name_var, arg_type)
        tc_state = module.comp_ctx.tc_state
        arg_expr = ir.Expr(type=tc_state.infer(arg_type), expr=arg_sym)
        block = [
            ignore(ir.Set(each, ir.Expr(type=t, expr=ir.Field(base=arg_expr, attr=attr))))
            for each, attr, t in record_fields
        ]
        block.append(quote_expr)
        ret_expr = ir.Fun("<quote>", module.comp_ctx.filename, arg_sym,
                          ir.Expr(expr=ir.Block(block), type=quote_expr.type))
        ret_type = te.Arrow(arg_type, quote_expr.type)
        return ir.Expr(expr=ret_expr, type=te.Forall(rho_group, (rho, ), ret_type))
