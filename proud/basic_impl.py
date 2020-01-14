from proud import composable_evaluator as ce
from proud import excs, sexpr
from proud import lowered_ir as ir
from proud.scope import Scope, Sym
from proud import types
from hybridts.tc_state import TCState
from hybridts import type_encoding as te
from collections import namedtuple
from contextlib import contextmanager
import typing


def ignore(expr: ir.BaseExpr) -> ir.Expr:
    return ir.Expr(type=types.unit_t, expr=expr)


unit = ir.Expr(type=types.unit_t, expr=ir.Const(()))


class CompilerCtx(
        namedtuple("CompilerCtx",
                   ["scope", "tc_state", "tenv", "filename", "path"])):
    scope: Scope
    tc_state: TCState
    tenv: typing.Dict[Sym, te.T]
    filename: str
    path: str

    def type_of_value(self, sym: Sym):
        return self.tenv[sym]

    def type_of_type(self, sym: Sym) -> te.T:
        tcs = self.tc_state
        t = tcs.infer(self.tenv[sym])
        if isinstance(t, te.App) and t.f is types.type_type:
            return t.arg
        what_i_want = te.InternalVar(is_rigid=False)
        tcs.unify(t, te.App(types.type_type, what_i_want))
        return what_i_want

    def value_of_type(self, sym: Sym):
        return self.tenv[sym]

    @classmethod
    def top(cls, filename, path):
        return cls(Scope.top(), TCState({}), {}, filename, path)


@contextmanager
def keep(self):
    comp = self.comp_ctx
    try:
        yield
    finally:
        self.comp_ctx = comp


def _sort_key(expr: ir.Expr):
    assert isinstance(expr.expr, Sym)
    return expr.expr.name


class Modular(ce.Eval_module, ce.Eval_loc, ce.Eval_define):
    def __init__(self, compiler_ctx: CompilerCtx):
        """
        NOTE!!: compiler_ctx should make subscope
        """
        self.comp_ctx = compiler_ctx

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
            module.comp_ctx = CompilerCtx(sub_scope, tc_state, tenv,
                                          prev_comp_ctx.filename,
                                          prev_comp_ctx.path)
            if type:
                my_type_for_unify = my_type = tc_state.infer(
                    Typing(module.comp_ctx).eval(type))
                if isinstance(my_type, te.Forall):
                    my_type_for_unify = my_type.poly_type
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
        if is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)
            module_type = tenv[mod_record_sym] = types.Var(loc=loc,
                                                           filename=filename,
                                                           name=path)
            out_append(
                ignore(
                    ir.Set(mod_record_sym,
                           ir.Expr(type=types.unit_t, expr=ir.Const(())))))
        with keep(module_eval):

            scope_inside = prev_comp_ctx.scope.sub_scope(hold_bound=True)
            comp_ctx = module_eval.comp_ctx = CompilerCtx(
                scope_inside, tc_state, tenv, filename, path)

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
                    nom_typename = Typing(comp_ctx).eval(typedef)
                else:
                    nom_typename = types.Nom(qual_typename,
                                             loc=loc,
                                             filename=filename)
                tenv[sym_typename] = te.App(types.type_type, nom_typename)

                in_append(ignore(ir.Loc(loc)))

                in_append(
                    ignore(
                        ir.Set(
                            sym_typename,
                            ir.Expr(type=nom_typename,
                                    expr=ir.Const(nom_typename)))))

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
            in_append(ir.Expr(type=module_type, expr=ir.Tuple(elts)))

        if not is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)
            tenv[mod_record_sym] = module_type

        out_append(
            ignore(
                ir.Set(mod_record_sym,
                       ir.Expr(type=module_type,
                               expr=ir.Block(block_inside)))))
        return ir.Expr(type=module_type, expr=ir.Block(block_outside))


type_map = {
    int: types.bigint_t,
    str: types.string_t,
    float: types.float_t,
    complex: types.complex_t,
    bool: types.bool_t
}


class Typing(ce.Eval_forall, ce.Eval_exist, ce.Eval_arrow, ce.Eval_imply,
             ce.Eval_tuple, ce.Eval_record, ce.Eval_list, ce.Eval_loc,
             ce.Eval_call, ce.Eval_type):
    def type(module, name, definition):
        assert name is None
        x = Express(module.comp_ctx).eval(definition).type
        ret = types.Var(module._loc, module.comp_ctx.filename, name="valtotype")
        module.comp_ctx.tc_state.unify(te.App(types.type_type, ret), x)
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

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
        self._loc = None


class Express(ce.Eval_let, ce.Eval_lam, ce.Eval_match, ce.Eval_annotate,
              ce.Eval_binary, ce.Eval_list, ce.Eval_tuple, ce.Eval_record,
              ce.Eval_call, ce.Eval_attr, ce.Eval_quote, ce.Eval_loc,
              ce.Eval_coerce, ce.Eval_literal, ce.Eval_type):
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
        raise NotImplementedError

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

        f_inst = None
        if isinstance(f_type, te.Implicit):
            f_inst = f_type.witness
            f_type = f_type.type
            assert not isinstance(f_type, te.Implicit)
        if isinstance(arg.type, te.Forall):
            if len(arg.type.fresh_vars) <= 1 or isinstance(
                    f_type, te.Arrow) and isinstance(f_type.arg, te.Forall):
                arg_map, arg_type = tc_state.inst_with_structure_preserved(
                    arg.type)
            else:
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
            me = ir.Fun(name, filename, freevars, arg_e, ret)
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
                    my_type_for_unify = my_type.poly_type
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

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
        self._loc = None

    def eval(self, x) -> ir.Expr:
        if sexpr.is_ast(x):
            hd, *args = x
            return ce.dispatcher[hd](*args)(self)

        if isinstance(x, str):
            my_type = self.comp_ctx.type_of_value(
                self.comp_ctx.scope.require(x))
            return ir.Expr(type=my_type, expr=x)

        raise TypeError(x)
