from proud import composable_evaluator as ce
from proud import sexpr, excs
from proud.scope import Scope, Sym
from proud import types
from hybridts.tc_state import TCState
from hybridts import type_encoding as te
from collections import namedtuple
from contextlib import contextmanager
import typing


class CompilerCtx(namedtuple("CompilerCtx", ["scope", "tc_state", "tenv"])):
    scope: Scope
    tc_state: TCState
    tenv: typing.Dict[Sym, te.T]

    def type_of_value(self, sym: Sym):
        return self.tenv[sym]

    def type_of_type(self, sym: Sym) -> te.T:
        t = self.tenv[sym]
        if t[0] is te.app_t and t[1] is types.type_type:
            return t[2]
        tcs = self.tc_state
        what_i_want = tcs.new_var()
        tcs.unify(t, types.type_app(types.type_type, what_i_want))
        return what_i_want

    def value_of_type(self, sym: Sym):
        return self.tenv[sym]


@contextmanager
def keep(self: 'Modular'):
    comp = self.comp_ctx
    try:
        yield
    finally:
        self.comp_ctx = comp


class Modular(ce.Eval_module):
    def __init__(self, path: str, filename: str, compiler_ctx: CompilerCtx):
        """
        NOTE!!: compiler_ctx should make subscope
        """
        self.path = path
        self.filename = filename
        self.comp_ctx = compiler_ctx

    def module(module_eval, is_rec, name, filename, stmts):
        prev_comp_ctx = module_eval.comp_ctx
        tc_state = prev_comp_ctx.tc_state
        tenv = prev_comp_ctx.tenv

        mod_record_sym: typing.Optional[Sym] = None
        block_outside = []
        block_inside = []
        if is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)
            block_outside.append((sexpr.set_k, mod_record_sym, None))

        with keep(module_eval):
            scope_inside = prev_comp_ctx.scope.sub_scope(hold_bound=True)
            comp_ctx = module_eval.comp_ctx = CompilerCtx(
                scope_inside, tc_state, tenv)

            module_eval.path = '{}.{}'.format(module_eval.path, name)
            module_eval.filename = filename

            exprs = []
            for stmt in stmts:
                if stmt[0] is sexpr.let_k:
                    exprs.append(stmt)
                    continue

                assert stmt[1] == sexpr.type_k
                _, typename, typedef = stmt
                loc, typename = sexpr.unloc(typename)
                typename = '{}.{}'.format(module_eval.path, typename)

                sym_typename = comp_ctx.scope.enter(typename)
                if typedef:
                    # type alias
                    nom_typename = Typing(comp_ctx).eval(typedef)
                else:
                    nom_typename = tc_state.mk_new_type(typename)

                block_inside.append((sexpr.set_k, sym_typename, nom_typename))

                tenv[sym_typename] = types.type_app(types.type_type,
                                                    nom_typename)

            syms = scope_inside.get_newest_bounds()
            # module is a record
            mod_expr = (sexpr.record_k, tuple(
                (s.name, s.name) for s in syms), None)
            exprs.reverse()
            for expr in exprs:
                mod_expr = (*expr, mod_expr)

            mod_row = te.row_from_list([(s.name, tenv[s]) for s in syms],
                                       te.empty_row)
            mod_record = types.record(mod_row)
            tenv[mod_record_sym] = mod_record
            mod_lowered_expr = Express(comp_ctx).eval(mod_expr)
            block_inside.append(mod_lowered_expr)

        if not is_rec:
            mod_record_sym = prev_comp_ctx.scope.enter(name)

        block_outside.append((sexpr.set_k, mod_record_sym,
                              (sexpr.block_k, tuple(block_inside))))
        return sexpr.block_k, tuple(block_outside)


type_map = {
    int: types.bigint_t,
    str: types.string_t,
    float: types.float_t,
    complex: types.complex_t
}


class Typing(ce.Eval_forall, ce.Eval_arrow, ce.Eval_imply, ce.Eval_tuple,
             ce.Eval_record, ce.Eval_list):
    def record(module, pairs, row):
        if row:
            row_t = te.poly_row(module.eval(row))

        else:
            row_t = te.empty_row

        fields = [(k, module.eval(v)) for k, v in pairs]
        row_t = te.row_from_list(fields, row_t)
        return types.record(row_t)

    def list(module, elts):
        list_t = types.list_t
        if not elts:
            return list_t

        try:
            [a] = elts
            list_elt_type = module.eval(a)
            return types.type_app(list_t, list_elt_type)
        except ValueError:
            raise excs.InvalidListType("List of {}".format(elts))

    def tuple(module, elts):
        return types.tuple(*map(module.eval, elts))

    def imply(module, arg, ret):
        arg = module.eval(arg)
        ret = module.eval(ret)
        return types.imply(arg, ret)

    def arrow(module, arg, ret):
        arg = module.eval(arg)
        ret = module.eval(ret)
        return types.arrow(arg, ret)

    def eval(self, x):
        if isinstance(x, Sym):
            return self.comp_ctx.type_of_type(x)
        if sexpr.is_ast(x):
            hd, *args = x
            return ce.dispatcher[hd](*args)(self)

        return type_map[type(x)]

    def forall(module, fresh_vars: typing.Tuple[str, ...], polytype):
        fresh_vars_ = frozenset(fresh_vars)
        if len(fresh_vars_) is not len(fresh_vars):
            raise excs.DuplicatedForallVar(fresh_vars_)
        fresh_vars = fresh_vars_
        return types.forall(fresh_vars, module.eval(polytype))

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx


class Express(ce.Eval_let, ce.Eval_lam, ce.Eval_match, ce.Eval_annotate,
              ce.Eval_binary, ce.Eval_list, ce.Eval_tuple, ce.Eval_record,
              ce.Eval_call, ce.Eval_attr, ce.Eval_quote):

    def let(module, is_rec, name, bound, body):
        pass
