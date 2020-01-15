from proud.core_lang import types
from proud.core_lang.scope import Scope, Sym
from collections import namedtuple
from hybridts.tc_state import TCState
from hybridts import type_encoding as te
from contextlib import contextmanager
import proud.core_lang.lowered_ir as ir
import typing
try:
    from proud.backend_interface import BackEnd
except ImportError:
    pass

__all__ = [
    'type_map', 'CompilerCtx', 'keep', 'Evaluator', 'anyway', 'unit', 'ignore'
]

type_map = {
    int: types.int_t,
    str: types.string_t,
    float: types.float_t,
    complex: types.complex_t,
    bool: types.bool_t
}


def ignore(ex: ir.BaseExpr):
    return ir.Expr(type=types.unit_t, expr=ex)


unit = ir.Expr(type=types.unit_t, expr=ir.Const(()))


class CompilerCtx(
        namedtuple(
            "CompilerCtx",
            ["scope", "tc_state", "tenv", "filename", "path", "backend"])):
    """
    You're supposed to init following types before compilation:
    - a type named unit
    - a type named int
    - a type named str
    - a type named bool
    - a type named float
    """
    scope: Scope
    tc_state: TCState
    tenv: typing.Dict[Sym, te.T]
    filename: str
    path: str  # it's not that path, it's the qualified name of current module.
    backend: 'BackEnd'

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
    def top(cls, filename, path, backend: 'BackEnd'):
        return cls(Scope.top(), TCState({}), {}, filename, path, backend)

    def with_scope(self, scope: Scope):
        return CompilerCtx(scope, self.tc_state, self.tenv, self.filename,
                           self.path, self.backend)

    def with_path(self, path: str):
        filename, _ = self.backend.search_module(path.split('.'))
        return CompilerCtx(self.scope, self.tc_state, self.tenv, filename, path, self.backend)


@contextmanager
def keep(self):
    comp = self.comp_ctx
    try:
        yield
    finally:
        self.comp_ctx = comp


def anyway(x: ir.BaseExpr):
    return ir.Expr(expr=x, type=types.type_type)


class Evaluator:
    comp_ctx: CompilerCtx
    _loc: typing.Optional[typing.Tuple[int, int]]

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
        self._loc = None
