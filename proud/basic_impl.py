from proud import composable_evaluator as ce
from proud import sexpr, excs
from proud.scope import Scope, Sym
from proud import types
from hybridts.tc_state import TCState
from hybridts import type_encoding as te
from collections import namedtuple
import typing


class CompilerCtx(namedtuple("CompilerCtx", ["scope", "tc_state", "tenv"])):
    scope: Scope
    tc_state: TCState
    tenv: typing.Dict[Sym, te.T]


class Modular(ce.Eval_module):
    def __init__(self, path: str, filename: str, compiler_ctx: CompilerCtx):
        """
        NOTE!!: compiler_ctx should make subscope
        """
        self.path = path
        self.filename = filename
        self.comp_ctx = compiler_ctx

    def module(module_eval, name, filename, stmts):
        path = module_eval.path

        filename, module_eval.filename = filename, module_eval.filename
        module_eval.path = '{}.{}'.format(path, name)

        exprs = []
        comp_ctx = module_eval.comp_ctx
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
                nom_typename = comp_ctx.tc_state.mk_new_type(typename)

            comp_ctx.tenv[sym_typename] = types.type_app(
                types.type_type, nom_typename)

        module_eval.filename = filename
        module_eval.path = path


class Typing(ce.Eval_forall, ce.Eval_arrow, ce.Eval_imply, ce.Eval_tuple,
             ce.Eval_record, ce.Eval_list):

    def record(module, pairs, row):
        if row:
            row_t = te.poly_row(module.eval(row))

        else:
            row_t = te.empty_row

        row_t = te.row_from_list([(k, module.eval(v)) for k, v in pairs], row_t)
        return types.record(row_t)

    def list(module, elts):
        list_t = types.nominal('list')
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
        hd, *args = x
        return ce.dispatcher[hd](*args)(self)

    def forall(module, fresh_vars: typing.Tuple[str, ...], polytype):
        fresh_vars_ = frozenset(fresh_vars)
        if len(fresh_vars_) is not len(fresh_vars):
            raise excs.DuplicatedForallVar(fresh_vars_)
        fresh_vars = fresh_vars_
        return types.forall(fresh_vars, module.eval(polytype))

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
