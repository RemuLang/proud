from proud.core_lang.scope import Sym, Scope
from proud.core_lang import lowered_ir as ir
from proud.core_lang import types
from proud.core_lang.check_valid import assure_not_generalised_record_value
from proud.helpers import Ref, MissingDict
from hybridts import type_encoding as te
from hybridts.tc_state import TCState
import typing


def _extract_implicits(type: te.T):
    """
    Use this after instantiations
    """
    implicit_types = []
    while isinstance(type, te.Implicit):
        implicit_types.append(type.witness)
        type = type.type
    return implicit_types, type


def _resolve_instance_(implicit_types: typing.List[te.T], scope: Scope, expr: ir.Expr):
    available_syms = list(scope.boundvars.values()) + list(scope.freevars.values())
    explicit_type = expr.type
    implicit_types.reverse()
    for implicit_t in implicit_types:
        instanced = ir.Expr(type=explicit_type, expr=expr.expr)
        expr.expr = ir.Instance(implicit_t, available_syms, instanced)


def resolve_instance_(scope: Scope, expr: ir.Expr):
    implicit_arg_types, expr.type = _extract_implicits(expr.type)
    if not implicit_arg_types:
        return
    _resolve_instance_(implicit_arg_types, scope, expr)


def generalise(tc_state: TCState,
               TV: typing.List[te.Var],
               type: te.T,
               others: typing.Iterable[te.T],
               loc=None,
               filename=None):
    forall = types.ForallScope(loc, filename)

    def default(_, ref=Ref(0)):
        ith = ref.contents
        ref.contents += 1
        return te.Fresh('a{}'.format(ith), forall)

    generalised_vars = MissingDict(default)
    infer = tc_state.infer
    unify = tc_state.unify

    for tv in TV:
        tv = infer(tv)
        if isinstance(tv, te.Var):
            _ = generalised_vars[tv]

    if generalised_vars:
        bounds = tuple(generalised_vars.values())
        for var, forall_bound in generalised_vars:
            unify(var, forall_bound)
        type = tc_state.infer(type)
        assure_not_generalised_record_value(type)
        type = te.Forall(forall, bounds, type)
        others = [te.Forall(forall, bounds, other) for other in others]
        return type, others
    return type, others


def _do_nothing(*xs):
    return xs


def implicit_and_generalise(scope: Scope,
                            tc_state: TCState,
                            expr: ir.Expr,
                            loc=None,
                            filename=None,
                            rigid=False):
    expr_type = tc_state.infer(expr.type)

    TV, expr_type = tc_state.inst_without_structure_preserved(expr_type, rigid=rigid)
    expr.type = expr_type
    if not TV:
        # don't have to generalise
        resolve_instance_(scope, expr)
        return _do_nothing

    resolve_instance_(scope, expr)

    def after_unification(*xs):
        expr.type, others = generalise(tc_state,
                                       list(TV.values()),
                                       expr.type,
                                       xs,
                                       loc=loc,
                                       filename=filename)
        return others

    return after_unification


def implicit_and_generalise_(scope: Scope,
                             tc_state: TCState,
                             expr: ir.Expr,
                             loc=None,
                             filename=None):
    expr_type = tc_state.infer(expr.type)

    TV, expr_type = tc_state.inst_without_structure_preserved(expr_type)
    expr.type = expr_type
    if not TV:
        # don't have to generalise
        resolve_instance_(scope, expr)
        return

    resolve_instance_(scope, expr)
    expr.type = generalise(tc_state,
                           list(TV.values()),
                           expr.type, [],
                           loc=loc,
                           filename=filename)
