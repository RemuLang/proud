from proud.core_lang import sexpr, lowered_ir as ir, composable_evaluator as ce, types
from proud.core_lang.scope import Sym, Scope
from proud.core_lang.modular_compiler import *
from proud.parser.parser_wrap import parse
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


class Modular(Evaluator, ce.Eval_module, ce.Eval_loc, ce.Eval_define,
              ce.Eval_type, ce.Eval_imp):
    def imp(module, qualname: list):
        comp_ctx = module.comp_ctx
        name = qualname[-1]
        sym = comp_ctx.scope.enter(name)
        module_finder = comp_ctx.backend.search_module(qualname)
        if module_finder.exist:
            mod_sym = module_finder.exist
            mod_type = comp_ctx.type_of_value(mod_sym)
            mod_exp = ir.Expr(expr=mod_sym, type=mod_type)
            return ignore(ir.Set(module_finder.exist, mod_exp))

        filename, get_code = module_finder.filename, module_finder.get_code
        path = '.'.join(qualname)
        new_modular = Modular(
            CompilerCtx(scope=comp_ctx.backend.mk_top_scope(),
                        tc_state=comp_ctx.tc_state,
                        tenv=comp_ctx.tenv,
                        filename=filename,
                        path=path,
                        backend=comp_ctx.backend))
        mod_ast = parse(get_code(), filename)
        mod_exp = new_modular.eval(mod_ast)
        comp_ctx.tenv[sym] = mod_exp.type
        comp_ctx.backend.remember_module(path, sym)
        return ignore(ir.Set(sym, mod_exp))

    def type(module, typename, typedef):
        comp_ctx = module.comp_ctx
        path = module.comp_ctx.path

        loc, typename = sexpr.unloc(typename)
        qual_typename = '{}.{}'.format(path, typename)

        sym_typename = comp_ctx.scope.enter(typename)
        if typedef:
            # type alias
            nom_typename = comp_ctx.tc_state.infer(
                Typing(comp_ctx).eval(typedef))
        else:
            nom_typename = types.Nom(qual_typename,
                                     loc=loc,
                                     filename=comp_ctx.filename)
        comp_ctx.tenv[sym_typename] = te.App(types.type_type, nom_typename)

        return ir.Expr(type=types.unit_t,
                       expr=ir.WrapLoc(
                           loc,
                           ir.Expr(type=types.unit_t,
                                   expr=ir.Set(
                                       sym_typename,
                                       ir.Expr(type=nom_typename,
                                               expr=ir.Const(nom_typename))))))

    def loc(module, location, contents):
        _, contents = sexpr.unloc(contents)
        exp = module.eval(contents)
        return ir.Expr(type=exp.type, expr=ir.WrapLoc(loc=location, expr=exp))

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
            me = scope.shadow(name)
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

        comp_ctx = module_eval.comp_ctx
        tc_state = comp_ctx.tc_state
        tenv = comp_ctx.tenv

        mod_record_sym: typing.Optional[Sym] = None
        block_outside: typing.List[ir.Expr] = []
        block_inside: typing.List[ir.Expr] = []
        in_append = block_inside.append
        out_append = block_outside.append
        filename = comp_ctx.filename
        loc, name = sexpr.unloc(name)
        path = '{}.{}'.format(comp_ctx.path, name)

        module_type = None
        unit_t = types.unit_t
        scope = comp_ctx.scope
        if is_rec:
            mod_record_sym = scope.enter(name)
            module_type = tenv[mod_record_sym] = types.Var(loc=loc,
                                                           filename=filename,
                                                           name=path)
            out_append(
                ir.Expr(type=unit_t,
                        expr=ir.Set(mod_record_sym,
                                    ir.Expr(type=unit_t, expr=ir.Const(())))))

        # exprs = []
        for stmt_located in stmts:
            loc, stmt = sexpr.unloc(stmt_located)
            if stmt[0] is sexpr.def_k:
                if stmt[1]:
                    # export
                    _loc, n = sexpr.unloc(stmt[2])
                    sym = scope.enter(n)
                    tenv[sym] = types.Var(_loc, filename, n)
            #     exprs.append(stmt)
            #     continue
            #
            # assert stmt[0] == sexpr.type_k
            # _, typename, typedef = stmt
            # _loc, typename = sexpr.unloc(typename)
            # qual_typename = '{}.{}'.format(path, typename)
            #
            # sym_typename = comp_ctx.scope.enter(typename)
            # if typedef:
            #     # type alias
            #     nom_typename = tc_state.infer(
            #         Typing(comp_ctx).eval(typedef))
            # else:
            #     nom_typename = types.Nom(qual_typename,
            #                              loc=loc,
            #                              filename=filename)
            # tenv[sym_typename] = te.App(types.type_type, nom_typename)
            #
            # in_append(
            #     ir.Expr(type=unit_t,
            #             expr=ir.WrapLoc(
            #                 loc,
            #                 ir.Expr(
            #                     type=unit_t,
            #                     expr=ir.Set(
            #                         sym_typename,
            #                         ir.Expr(
            #                             type=nom_typename,
            #                             expr=ir.Const(nom_typename)))))))

            # module is a record

        for stmt in stmts:
            in_append(module_eval.eval(stmt))

        syms = scope.get_newest_bounds()

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
            mod_record_sym = comp_ctx.scope.enter(name)
            tenv[mod_record_sym] = module_type

        out_append(
            ir.Expr(type=unit_t,
                    expr=ir.Set(
                        mod_record_sym,
                        ir.Expr(type=module_type,
                                expr=ir.Block(block_inside)))))
        return ir.Expr(type=module_type, expr=ir.Block(block_outside))
