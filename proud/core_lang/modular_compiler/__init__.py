"""
    function application:
            T |- f : (Q) a -> b
            T |- arg : c
            a[tau/Q] >= c
        procedure:
        1. inst ((Q) a -> b) and c
        2. inst a[tau/Q] with rigid type variables, got a', and record the instantiated variables A
            -- hints for type classes: keep implicit type in a', don't separate it from a'.
        3. inst c with flexible type variables, got c', record the instantiated variables B.
            -- hints for type classes: extract out implicit types in c', separate it from c'.
        4. C = A ++ B, make a new type variable ret'
        5. unify (a', c'), unify(b[tau/Q], ret')
        6. C' = {infer(tv) | tc in C, infer(v) isa type variable} (it's a set!!)
        7. if C' is not empty:
                for each tv in ret', if tv in C', subst a distinct bound variable in,
                all those bound variables make up of Q'.
                ret' <- Q' ret'[Q'/C']
                ------------------------------------------------------------------------
                Further, for type classes, we need to update the type of arg:
                c <- Q'' c'[Q''/C'].                                       <------------
                    Why must be there some qualifiers Q''?                              |
                    Because the bound type variables of ret' is introduced by c.        |
                    -- more hints for type classes:                                     |
                    if any implicit type(I) in c', it'll get updated, too, by this step |
                    , and we should update the term arg:
                        arg <- instance(I, (term level bound variables), arg)
                        where the bound variables will be used for instance resolution.

                P.S: All type variables instantiated by Q keep mono.
                P.S: the term f can also have implicit types, but it's trivial here.
                ---------------------------------------------------------------------
           else, monotype, just return ret'
        8. GOOD NEWS: we don't have to care about the type of a', for both type infer and code gen


    let binding:
        for "let a = b [and ...] in c", it works like "f = fun a -> c, f b"
        T |- a: t1
        T |- b: t2
        procedure:
        1. instantiate t1 with rigid tvs, got t1', record the inst tvs A
        2. instantiate t2 with flexible tvs, got t2', record the inst tvs B
        3. C = A ++ B
        4. unify(t1', t2'), got env T', T' |- c : t3'
        5. C' = {infer(tv) | tc in C, infer(v) isa type variable} (it's a set!!)
        6. if C' is not empty:
              polymorphise t3' with C' (and t2', if we're to support type class)
           else just return t3'
        -- hints for type annotations
           for poly type like forall a. T, a does not enter the scope.
           but for exist a. T, it's okay for a to enter the scope

    """

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
    'type_map', 'CompilerCtx', 'keep', 'Evaluator', 'anyway', 'unit', 'ignore',
    'wrap_loc', 'Interpreter', 'CompilerLocalContext', 'CompilerGlobalContext', 'Location'
]
Location = typing.Tuple[int, int]

type_map = {
    int: types.int_t,
    str: types.string_t,
    float: types.float_t,
    complex: types.complex_t,
    bool: types.bool_t
}


def ignore(ex: ir.BaseExpr):
    return ir.Expr(type=types.unit_t, expr=ex)


def anyway(x: ir.BaseExpr):
    return ir.Expr(expr=x, type=types.anyway_type)


def wrap_loc(loc: Location, expr: ir.Expr):
    return ignore(ir.WrapLoc(loc, expr))


unit = ir.Expr(type=types.unit_t, expr=ir.Const(()))


## Compiler contexts
class CompilerGlobalContext:
    tc_state: TCState
    tenv: typing.Dict[Sym, te.T]
    backend: 'BackEnd'

    def __init__(self, tc_state: TCState, tenv: typing.Dict[Sym, te.T],
                 backend: 'BackEnd'):
        self.tc_state = tc_state
        self.tenv = tenv
        self.backend = backend

    @classmethod
    def create(cls, backend: 'BackEnd'):
        return cls(TCState({}), {}, backend)


class CompilerLocalContext:
    scope: Scope
    filename: str
    path: str
    location: typing.Optional[Location]

    def __init__(self,
                 scope: Scope,
                 filename: str,
                 path: str,
                 location: typing.Optional[Location] = None):
        self.scope = scope
        self.filename = filename
        self.path = path
        self.location = location

    @classmethod
    def top(cls, filename: str, path: str, location=(0, 0)):
        return cls(Scope.top(), filename, path, location)

    @contextmanager
    def resume_scope(self):
        prev_scope = self.scope
        try:
            yield
        finally:
            self.scope = prev_scope


## Interpreter base class
class Interpreter:
    clc: CompilerLocalContext

    def __init__(self, clc: CompilerLocalContext):
        self.clc = clc

    def eval(self, node: tuple):
        raise NotImplementedError


class CompilerCtx(
        namedtuple("CompilerCtx",
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
        return CompilerCtx(scope, self.tc_state, self.tenv, self.filename, self.path,
                           self.backend)


@contextmanager
def keep(self):
    comp = self.comp_ctx
    try:
        yield
    finally:
        self.comp_ctx = comp


class Evaluator:
    comp_ctx: CompilerCtx
    _loc: typing.Optional[typing.Tuple[int, int]]

    def __init__(self, comp_ctx: CompilerCtx):
        self.comp_ctx = comp_ctx
        self._loc = None
