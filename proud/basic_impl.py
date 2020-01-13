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
        t = self.tenv[sym]
        if isinstance(t, te.App) and t.f is types.type_type:
            return t.arg
        tcs = self.tc_state
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


class Modular(ce.Eval_module, ce.Eval_loc, ce.Eval_define):
    def __init__(self, compiler_ctx: CompilerCtx):
        """
        NOTE!!: compiler_ctx should make subscope
        """
        self.comp_ctx = compiler_ctx

    def loc(module, location, contents):
        assert sexpr.is_ast(contents) and contents[0] is sexpr.module_k
        a = module.eval(contents)
        return ir.Expr(type=a.type, expr=ir.WrapLoc(location, a))

    def eval(self, x) -> ir.Expr:
        assert sexpr.is_ast(x)
        hd, *args = x
        return ce.dispatcher[hd](*args)(self)

    def define(self, export, name, type, bound):
        module = Express(self.comp_ctx)
        loc, name = sexpr.unloc(name)
        prev_comp_ctx = module.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv
        scope = module.comp_ctx.scope
        sub_scope = scope.sub_scope()
        me = (scope.require if export else scope.shadow)(name)
        bound = module.eval(bound)
        with keep(module):
            module.comp_ctx = CompilerCtx(sub_scope, tc_state, tenv, prev_comp_ctx.filename, prev_comp_ctx.path)
            if type:
                my_type = Typing(module.comp_ctx).eval(type)
                if isinstance(my_type, te.Forall):
                    my_type_for_unify = my_type.poly_type
                if isinstance(my_type, te.Implicit):
                    my_type_for_unify = my_type.type
            else:
                my_type = my_type_for_unify = types.Var(loc, prev_comp_ctx.filename, name=name)

            tenv[me] = my_type
            # TODO: instantiate and generalise like function application
            tc_state.unify(my_type_for_unify, bound.type)

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
                        scope_inside.enter(sexpr.unloc(stmt[2])[1])
                    exprs.append(stmt)
                    continue

                assert stmt[0] == sexpr.type_k
                _, typename, typedef = stmt
                _loc, typename = sexpr.unloc(typename)
                typename = '{}.{}'.format(path, typename)

                sym_typename = comp_ctx.scope.enter(typename)
                if typedef:
                    # type alias
                    nom_typename = Typing(comp_ctx).eval(typedef)
                else:
                    nom_typename = types.Nom(typename,
                                             loc=loc,
                                             filename=filename)

                in_append(ignore(ir.Loc(loc)))

                in_append(
                    ignore(
                        ir.Set(
                            sym_typename,
                            ir.Expr(type=nom_typename,
                                    expr=ir.Const(nom_typename)))))

                tenv[sym_typename] = te.App(types.type_type, nom_typename)

            syms = scope_inside.get_newest_bounds()
            # module is a record
            in_extend(map(module_eval.eval, exprs))

            mod_row = te.row_from_list([(s.name, tenv[s]) for s in syms],
                                       te.empty_row)
            if module_type:
                tc_state.unify(module_type, te.Record(mod_row))
            else:
                module_type = te.Record(mod_row)
            in_append(
                ir.Expr(type=module_type,
                        expr=ir.Tuple([
                            ir.Expr(type=tenv[sym], expr=sym) for sym in syms
                        ])))

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
    complex: types.complex_t
}


class Typing(ce.Eval_forall, ce.Eval_exist, ce.Eval_arrow, ce.Eval_imply,
             ce.Eval_tuple, ce.Eval_record, ce.Eval_list, ce.Eval_loc):
    def record(module, pairs, row):
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
        for n in bound_vars:
            var = module.comp_ctx.scope.enter(n)
            tvar = types.Var(module._loc, module.comp_ctx.filename)
            module.comp_ctx.tenv[var] = te.App(types.type_type, tvar)

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
              ce.Eval_call, ce.Eval_attr, ce.Eval_quote, ce.Eval_loc):
    def quote(module, contents):
        raise NotImplementedError

    def attr(module, base, attr_name: str):
        raise NotImplementedError

    def record(module, pairs, row):
        raise NotImplementedError

    def tuple(module, elts):
        raise NotImplementedError

    def list(module, elts):
        raise NotImplementedError

    def binary(module, head, tl: typing.Tuple[tuple, ...]):
        raise NotImplementedError

    def annotate(module, var, type):
        raise NotImplementedError

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

        paths_of_argt, freshs_of_argt, arg_t = tc_state.inst_with_fresh_map(arg.type)
        ret_t = types.Var(module._loc, module.comp_ctx.filename, name="ret")
        paths_of_ft, freshs_of_ft, f_t = tc_state.inst_with_fresh_map(f.type)

        # type class
        arg_inst = None
        if isinstance(arg_t, te.Implicit):
            arg_inst = arg_t.witness
            arg_t = arg_t.type

        f_inst = None
        if isinstance(f_t, te.Implicit):
            f_inst = f_t.witness
            f_t = f_t.type

        is_generalised = False
        if paths_of_argt and paths_of_ft:
            # e.g.,
            # forall a b. a -> b
            # forall a. (int -> a) -> a -> forall b. (int -> b) -> b
            # forall b.  b
            if freshs_of_ft and isinstance(f_t, te.Arrow):
                inst_arrow = te.Arrow(arg_t, ret_t)
                tc_state.unify(inst_arrow, f_t)
                gen_map = {}
                forall_scope = types.ForallScope(module._loc,
                                                 module.comp_ctx.filename)
                for path, fresh in zip(paths_of_argt, freshs_of_argt):
                    _, token = tc_state.path_infer(path)
                    if not isinstance(token, te.Var):
                        continue
                    if not gen_map.get(token):
                        gen_map[token] = te.Fresh(fresh.name, forall_scope)

                if gen_map:
                    is_generalised = True
                    fresh_vars = tuple(gen_map.values())

                    def subst_and_unify(_, t: te.T):
                        return (), gen_map.get(t, t)

                    ret_t = tc_state.infer(ret_t)
                    arg_t = tc_state.infer(arg_t)
                    f_t = tc_state.infer(f_t)

                    ret_t = te.pre_visit(subst_and_unify)((), tc_state.infer(ret_t))
                    arg_t = te.pre_visit(subst_and_unify)((), tc_state.infer(arg_t))
                    f_t = te.pre_visit(subst_and_unify)((), tc_state.infer(f_t))

                    arg.type = te.Forall(forall_scope, fresh_vars, arg_t)
                    f.type = te.Forall(forall_scope, fresh_vars, f_t)
                    ret_t = te.Forall(forall_scope, fresh_vars, ret_t)

        if not is_generalised:
            inst_arrow = te.Arrow(arg_t, ret_t)
            tc_state.unify(inst_arrow, f_t)

        if f_inst:
            f = ir.Instance(f_inst, module.comp_ctx.scope, f)

        if arg_inst:
            arg = ir.Instance(arg_inst, module.comp_ctx.scope, arg)

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
                arg_t = Typing(module.comp_ctx).eval(type)
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
            if is_rec:
                me = sub_scope.enter(name)
                bound = module.eval(bound)
            else:
                bound = module.eval(bound)
                me = sub_scope.enter(name)

            if type:
                my_type = Typing(module.comp_ctx).eval(type)
            else:
                my_type = types.Var(loc, prev_comp_ctx.filename, name=name)

            tenv[me] = my_type

            # TODO: instantiate and generalise like function application
            tc_state.unify(my_type, bound.type)
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
            my_type = self.comp_ctx.type_of_value(self.comp_ctx.scope.require(x))
            return ir.Expr(type=my_type, expr=x)

        my_type = type_map[type(x)]
        return ir.Expr(type=my_type, expr=ir.Const(x))
