from proud.core_lang.scope import Scope
from proud.core_lang import lowered_ir as ir
from proud.core_lang.types import ForallScope
from proud.core_lang.check_valid import assure_not_generalised_record_value
from proud.unification import type_encode as te
from proud.unification.interface import TCState
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


def generalise_type(tc_state: TCState, type: te.T, level: te.Level, loc=None, filename=None):
    bounds = set()
    forall = ForallScope(loc, filename)
    cnt = 0
    TV: typing.Set[te.Var] = te.ftv(tc_state.infer(type))
    for tv in TV:
        # assert not tv.belong_to.final, tv.belong_to.final
        if tv.belong_to.final:
            continue
        belong_to = tv.belong_to
        # print(level, tv, belong_to.show_linked_by())
        if belong_to.is_closed_after(level):
            bound = te.Bound("a{}".format(cnt), forall)
            cnt += 1
            belong_to.final = bound
            # belong_to.destroy_()
            bounds.add(bound)

    if not cnt:
        return type
    # TODO, check if valid record type
    return te.Forall(forall, tuple(bounds), type)
