from proud.core_lang import sexpr, lowered_ir as ir, composable_evaluator as ce, types
from proud.core_lang.scope import Scope, Sym
from proud.core_lang.polymorphisms import generalise_type, resolve_instance_
from proud.core_lang.modular_compiler import *
from proud.unification import type_encode as te
from proud import excs
import typing


def make(cgc: CompilerGlobalContext):
    tc_state = cgc.tc_state
    tenv = cgc.tenv
    backend = cgc.backend
    infer = tc_state.infer
    unify = tc_state.unify
    inst = tc_state.inst

    Typing = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)
    Quote = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)

    def link_typing(evaluators):
        nonlocal Typing
        Typing = evaluators['Typing']

    def link_quote(evaluators):
        nonlocal Quote
        Quote = evaluators['Quote']

    def type_of_value(sym: Sym):
        return tenv[sym]

    def new_var(clc: CompilerLocalContext, name: str):
        return tc_state.new_var(Var=types.Var, loc=clc.location, filename=clc.filename, name=name)

    class Express(Interpreter, ce.Eval_let, ce.Eval_lam, ce.Eval_annotate, ce.Eval_tuple, ce.Eval_record, ce.Eval_call,
                  ce.Eval_attr, ce.Eval_quote, ce.Eval_loc, ce.Eval_coerce, ce.Eval_literal, ce.Eval_type,
                  ce.Eval_extern, ce.Eval_ite, ce.Eval_tellme):

        def eval_with_implicit(self, term):
            expr = self.eval(term)
            expr.type = inst(infer(expr.type))[1]
            resolve_instance_(self.clc.scope, expr)
            return expr

        def tellme(module, name, body, *, backend=backend):
            sym = module.clc.scope.require(name)
            backend.types_to_show.append((module.clc.location, module.clc.filename, sym))
            return module.eval(body)

        def extern(module, foreign_code):
            clc = module.clc
            return ir.Expr(expr=ir.Extern(foreign_code), type=new_var(clc, 'extern'))

        def ite(module, cond, true_clause, else_clause):
            level = tc_state.push_level()
            cond = module.eval_with_implicit(cond)

            tc_state.unify(cond.type, types.bool_t)

            tc = module.eval_with_implicit(true_clause)
            ec = module.eval_with_implicit(else_clause)

            tc_state.unify(tc.type, ec.type)
            clc = module.clc
            ec.type = tc.type = generalise_type(tc_state, tc.type, level=level, loc=clc.location, filename=clc.filename)
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
            return ir.Expr(expr=ir.Coerce(expr), type=new_var(module.clc, "coerce"))

        def attr(module, base, attr_name: str):
            level = tc_state.push_level()
            base = module.eval_with_implicit(base)
            clc = module.clc
            var = new_var(clc, 'attr')
            tho = new_var(clc, 'ρ')
            record_type = te.Record(te.row_from_list([(attr_name, var)], te.RowPoly(tho)))

            # TODO: how to deal with a polymorphic record value?
            # e.g., forall a. {x : ... | a}
            unify(record_type, base.type)
            var = infer(var)
            var = generalise_type(tc_state, var, level=level, loc=clc.location, filename=clc.filename)
            return ir.Expr(expr=ir.Field(base=base, attr=attr_name), type=var)

        def record(module, pairs, row):
            clc = module.clc
            filename = clc.filename
            location = clc.location
            level = tc_state.push_level()
            kv = []
            for each in pairs:
                loc, (attr_name, expr) = sexpr.unloc(each)
                expr = module.eval_with_implicit(expr)
                kv.append((attr_name, expr))

            kv.sort(key=lambda tp: tp[0])
            elts = [elt for _, elt in kv]
            left = ir.Tuple(elts)
            fields = []
            for k, v in kv:
                v_type = tc_state.infer(v.type)
                if isinstance(v_type, te.Forall):
                    v_type = v_type.poly_type
                fields.append((k, v_type))

            if not row:
                mono = te.Record(te.row_from_list(fields, te.empty_row))
                mono = generalise_type(tc_state, mono, level=level, loc=location, filename=filename)
                return ir.Expr(expr=ir.Tuple([anyway(left), anyway(ir.Tuple([]))]), type=mono)

            right = module.eval_with_implicit(row)
            poly = tc_state.infer(te.Record(te.row_from_list(fields, te.RowPoly(right.type))))
            poly = generalise_type(tc_state, poly, level=level, loc=location, filename=filename)
            return ir.Expr(expr=ir.Merge(left=anyway(left), right=right), type=poly)

        def tuple(module, elts):
            if not elts:
                return ir.Expr(expr=ir.Const(()), type=types.unit_t)
            if len(elts) is 1:
                return module.eval(elts[0])
            level = tc_state.push_level()
            elts = list(map(module.eval_with_implicit, elts))
            type = te.Tuple(tuple(e.type for e in elts))
            loc = module.clc.location
            filename = module.clc.filename
            type = generalise_type(tc_state, type, loc=loc, filename=filename, level=level)
            return ir.Expr(expr=ir.Tuple(elts), type=type)

        def annotate(module, var, type):
            level = tc_state.push_level()
            clc = module.clc
            var = module.eval_with_implicit(var)
            type = infer(Typing(clc).eval(type))
            type = inst(type, rigid=True)[1]
            tc_state.unify(type, var.type)
            var.type = generalise_type(tc_state, type, loc=clc.location, filename=clc.filename, level=level)
            return var

        def loc(module, location, contents):
            module.clc.location = location
            a = module.eval(contents)
            return ir.Expr(type=a.type, expr=ir.WrapLoc(location, a))

        def call(module, f, arg):
            clc = module.clc
            f = module.eval(f)
            f.type = infer(f.type)

            to_mono = None
            to_poly = None
            func_type = f.type

            # row polymorphisms
            while isinstance(func_type, te.Implicit):
                func_type = func_type.type
            if isinstance(func_type, te.Arrow):
                # layout of record might change correspondingly.
                to_mono = func_type.ret
                to_poly = func_type.arg

            f.type = inst(f.type)[1]
            resolve_instance_(clc.scope, f)
            level = tc_state.push_level()
            arg = module.eval_with_implicit(arg)
            ret_t = new_var(clc, name='ret')
            inst_arrow = te.Arrow(arg.type, ret_t)
            tc_state.unify(inst_arrow, f.type)
            ret_t = generalise_type(tc_state, ret_t, loc=clc.location, filename=clc.filename, level=level)

            if to_poly:
                arg = ir.Expr(type=arg.type, expr=ir.Polymorphization(layout_type=to_poly, expr=arg))

            ret = ir.Expr(type=ret_t, expr=ir.Invoke(f, arg))

            if to_mono:
                ret = ir.Expr(type=ret_t, expr=ir.Momomorphization(layout_type=to_mono, expr=ret))

            return ret

        def lam(module, arg, type, ret):
            loc, name = sexpr.unloc(arg)
            clc = module.clc
            outer_scope = clc.scope
            filename = clc.filename
            path = clc.path

            if type:
                type_arg = tc_state.infer(Typing(clc).eval(type))
            else:
                type_arg = new_var(clc, name)

            with clc.resume_scope():
                level = tc_state.push_level()
                inner_scope = outer_scope.sub_scope(hold_bound=True)
                clc.scope = inner_scope
                sym_arg = inner_scope.enter(name)
                tenv[sym_arg] = type_arg

                type_inst_arg = te.fresh(tc_state.new_var, type_arg)

                type_inst_arg = inst(type_inst_arg, rigid=True)[1]

                ret = module.eval_with_implicit(ret)
                lam_type = te.Arrow(type_inst_arg, ret.type)
                lam_type = generalise_type(tc_state, lam_type, loc=loc, filename=filename, level=level)

            name = "{} |{}|".format(path, name)
            sym_arg = ir.Fun(name, filename, sym_arg, ret)
            return ir.Expr(type=lam_type, expr=sym_arg)

        def let(module, is_rec, seq, body):
            clc = module.clc
            filename = clc.filename
            with clc.resume_scope():
                outer_scope = clc.scope = clc.scope.sub_scope()
                types = []
                for loc, name, annotation, _ in seq:
                    _, name = sexpr.unloc(name)
                    sym_bind = outer_scope.enter(name)
                    if annotation:
                        type_bind = Typing(clc).eval(annotation)
                    else:
                        type_bind = new_var(clc, name)
                    types.append(type_bind)
                    if is_rec:
                        tenv[sym_bind] = type_bind

                block = []
                for (loc, name, annotation, bound), sym_type in zip(seq, types):
                    _, name = sexpr.unloc(name)
                    with clc.resume_scope():
                        level = tc_state.push_level()
                        inner_scope = clc.scope = outer_scope.sub_scope()
                        if is_rec:
                            sym_bind = inner_scope.require(name)
                        else:
                            sym_bind = inner_scope.enter(name)
                            tenv[sym_bind] = sym_type
                        t_inst = te.fresh(tc_state.new_var, sym_type)
                        t_inst = inst(t_inst, rigid=True)[1]
                        bound = module.eval_with_implicit(bound)
                        tc_state.unify(t_inst, bound.type)
                        gen_type = generalise_type(tc_state, t_inst, loc=loc, filename=clc.filename, level=level)
                        bound.type = gen_type
                    unify(gen_type, sym_type)
                    block.append(ignore(ir.Set(sym_bind, bound)))

                level = tc_state.push_level()
                body = module.eval_with_implicit(body)
                block.append(body)
                body.type = generalise_type(tc_state, body.type, loc=clc.location, filename=clc.filename, level=level)
                return ir.Expr(type=body.type, expr=ir.Block(block))

        def eval_sym(self, x: str) -> ir.Expr:
            scope = self.clc.scope
            my_type = type_of_value(scope.require(x))
            var = scope.require(x)
            return ir.Expr(type=my_type, expr=var)

        def eval(self, x) -> ir.Expr:
            try:
                if sexpr.is_ast(x):
                    hd, *args = x
                    return ce.dispatcher[hd](*args)(self)

                if isinstance(x, str):
                    return self.eval_sym(x)

                raise TypeError(x)
            except excs.StaticCheckingFailed:
                raise
            except Exception as e:
                raise excs.StaticCheckingFailed(e, self.clc)

        def quote(module, contents):
            clc = module.clc
            quotation = Quote(clc)
            quote_expr = quotation.eval(contents)
            quote_scope: Scope = quotation.clc.scope
            sym_arg = quote_scope.enter(".external")
            name_var = new_var(clc, name="external")
            tenv[sym_arg] = name_var

            record_fields = []
            for attr_name, each in quote_scope.freevars.items():
                record_fields.append((each, attr_name, tenv[each]))

            rho_group = types.ForallScope(clc.location, filename=clc.filename)
            rho = te.Bound('ρ', rho_group)
            type_arg = te.Record(te.row_from_list([(attr, t) for _, attr, t in record_fields], te.RowPoly(rho)))
            tc_state.unify(name_var, type_arg)
            exp_arg = ir.Expr(type=tc_state.infer(type_arg), expr=sym_arg)
            block = [ignore(ir.Set(each, ir.Expr(type=t, expr=ir.Field(base=exp_arg, attr=attr)))) for each, attr, t in
                     record_fields]
            block.append(quote_expr)
            ret_expr = ir.Fun("<quote>", clc.filename, sym_arg, ir.Expr(expr=ir.Block(block), type=quote_expr.type))
            ret_type = te.Arrow(type_arg, quote_expr.type)
            return ir.Expr(expr=ret_expr, type=te.Forall(rho_group, (rho,), ret_type))

    return Express, [link_quote, link_typing]
