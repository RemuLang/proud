from hybridts import type_encoding as te
from proud.core_lang.scope import Sym, Scope
from proud.core_lang import lowered_ir as ir
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
