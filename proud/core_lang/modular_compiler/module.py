from proud.core_lang import sexpr, lowered_ir as ir, composable_evaluator as ce, types
from proud.core_lang.scope import Sym
from proud.core_lang.modular_compiler import *
from hybridts import type_encoding as te
import typing

try:
    # noinspection PyUnresolvedReferences
    from proud.core_lang.modular_compiler.expression import Express
    # noinspection PyUnresolvedReferences
    from proud.core_lang.modular_compiler.typing import Typing
except ImportError:
    pass


def _sort_key(expr: ir.Expr):
    assert isinstance(expr.expr, Sym)
    return expr.expr.name


class Modular(Evaluator, ce.Eval_module, ce.Eval_loc, ce.Eval_define, ce.Eval_type):
    def loc(module, location, contents):
        assert sexpr.is_ast(contents) and contents[0] is sexpr.module_k
        return module.eval(contents)

    def eval(self, x) -> ir.Expr:
        assert sexpr.is_ast(x)
        hd, *args = x
        a = ce.dispatcher[hd](*args)(self)
        a.type = self.comp_ctx.tc_state.infer(a.type)
        return a

    def define(self, export, name, type, bound):
        module = Express(self.comp_ctx)
        loc, name = sexpr.unloc(name)

        prev_comp_ctx = module.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv
        scope = module.comp_ctx.scope
        if export:
            me = scope.require(name)
            name_var = tenv[me]
        else:
            me = scope.enter(name)
            name_var = types.Var(loc, module.comp_ctx.filename, name)
            tenv[me] = name_var

        sub_scope = scope.sub_scope()

        with keep(module):
            module.comp_ctx = prev_comp_ctx.with_scope(sub_scope)
            if type:
                my_type_for_unify = my_type = tc_state.infer(
                    Typing(module.comp_ctx).eval(type))
                if isinstance(my_type, te.Forall):
                    my_type_for_unify = types.remove_bound_scope(my_type)
                tc_state.unify(name_var, my_type)
            else:
                my_type = my_type_for_unify = name_var

            bound = module.eval(bound)
            bound_type = tc_state.infer(bound.type)

            if type:
                _, bound_type = tc_state.inst_without_structure_preserved(
                    bound_type)
                bound.type = bound_type

            tc_state.unify(my_type_for_unify, bound_type)

            my_exp = ir.Set(me, bound)
            return ir.Expr(type=my_type, expr=my_exp)

    def module(module_eval, is_rec, name, stmts, loc=None):

        prev_comp_ctx = module_eval.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv

        mod_record_sym: typing.Optional[Sym] = None
        block_outside: typing.List[ir.Expr] = []
        block_inside: typing.List[ir.Expr] = []
        in_append = block_inside.append
        in_extend = block_inside.extend
        out_append = block_outside.append
        filename = prev_comp_ctx.filename
        loc, name = sexpr.unloc(name)
        path = '{}.{}'.format(prev_comp_ctx.path, name)

        module_type = None
        unit_t = types.unit_t

        if is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)
            module_type = tenv[mod_record_sym] = types.Var(loc=loc,
                                                           filename=filename,
                                                           name=path)
            out_append(
                ir.Expr(type=unit_t,
                        expr=ir.Set(mod_record_sym,
                                    ir.Expr(type=unit_t, expr=ir.Const(())))))
        with keep(module_eval):

            scope_inside = prev_comp_ctx.scope.sub_scope(hold_bound=True)
            comp_ctx = module_eval.comp_ctx = prev_comp_ctx.with_scope(
                scope_inside)

            exprs = []
            for stmt_located in stmts:
                loc, stmt = sexpr.unloc(stmt_located)
                if stmt[0] is sexpr.def_k:
                    if stmt[1]:
                        # export
                        _loc, n = sexpr.unloc(stmt[2])
                        sym = scope_inside.enter(n)
                        tenv[sym] = types.Var(_loc, filename, n)
                    exprs.append(stmt)
                    continue
                assert stmt[0] == sexpr.type_k
                _, typename, typedef = stmt
                _loc, typename = sexpr.unloc(typename)
                qual_typename = '{}.{}'.format(path, typename)

                sym_typename = comp_ctx.scope.enter(typename)
                if typedef:
                    # type alias
                    nom_typename = tc_state.infer(
                        Typing(comp_ctx).eval(typedef))
                else:
                    nom_typename = types.Nom(qual_typename,
                                             loc=loc,
                                             filename=filename)
                tenv[sym_typename] = te.App(types.type_type, nom_typename)

                in_append(
                    ir.Expr(type=unit_t,
                            expr=ir.WrapLoc(
                                loc,
                                ir.Expr(
                                    type=unit_t,
                                    expr=ir.Set(
                                        sym_typename,
                                        ir.Expr(
                                            type=nom_typename,
                                            expr=ir.Const(nom_typename)))))))

            syms = scope_inside.get_newest_bounds()
            # module is a record
            in_extend(map(module_eval.eval, exprs))

            mod_row = te.row_from_list([(s.name, tenv[s]) for s in syms],
                                       te.empty_row)
            if module_type:
                tc_state.unify(module_type, te.Record(mod_row))
            else:
                module_type = te.Record(mod_row)
            elts = [ir.Expr(type=tenv[sym], expr=sym) for sym in syms]
            elts.sort(key=_sort_key)
            in_append(
                ir.Expr(type=module_type,
                        expr=ir.Tuple(
                            [anyway(ir.Tuple(elts)),
                             anyway(ir.Tuple([]))])))

        if not is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)
            tenv[mod_record_sym] = module_type

        out_append(
            ir.Expr(type=unit_t,
                    expr=ir.Set(
                        mod_record_sym,
                        ir.Expr(type=module_type,
                                expr=ir.Block(block_inside)))))
        return ir.Expr(type=module_type, expr=ir.Block(block_outside))
