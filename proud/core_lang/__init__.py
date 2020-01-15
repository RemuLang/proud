from proud import types
from proud.scope import Scope, Sym
from collections import namedtuple
from hybridts.tc_state import TCState
from hybridts import type_encoding as te
from contextlib import contextmanager
import proud.lowered_ir as ir
import typing
__all__ = [
    'ignore', 'unit', 'CompilerCtx', 'keep', 'type_map', 'Evaluator', 'anyway'
]

type_map = {
    int: types.bigint_t,
    str: types.string_t,
    float: types.float_t,
    complex: types.complex_t,
    bool: types.bool_t
}


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


def anyway(x: ir.BaseExpr):
    return ir.Expr(expr=x, type=types.unit_t)


class Evaluator:
    comp_ctx: CompilerCtx
    _loc: typing.Optional[typing.Tuple[int, int]]

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
        self._loc = None
