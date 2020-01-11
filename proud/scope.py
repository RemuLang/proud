import typing as t
from collections import namedtuple

T = t.TypeVar('T')


class Undef(Exception):
    def __init__(self, name):
        self.name = name


class BindTwice(Exception):
    def __init__(self, name):
        self.name = name


class NoAllocatorForBound(Exception):
    def __init__(self, name):
        self.name = name


class Ref(t.Generic[T]):
    contents: T

    def __init__(self, v: T):
        self.contents = v


class Sym(namedtuple("Sym", ["name", "uid"])):
    name: str
    uid: object


class Scope(
        namedtuple("Scope",
                   ["freevars", "boundvars", "hold_bound", "parent"])):
    freevars: t.Dict[str, Sym]
    boundvars: t.Dict[str, Sym]
    hold_bound: bool
    parent: t.Optional['Scope']

    def require(self, name: str) -> Sym:
        ...

    def enter(self, name: str) -> Sym:
        ...

    def sub_scope(self, hold_bound=False):
        ...


def require(scope: Scope, name: str) -> Sym:
    var = scope.boundvars.get(name, None)
    if var is not None:
        return var
    var = scope.freevars.get(name, None)
    if var is not None:
        return var
    if scope.parent:
        var = require(scope.parent, name)
        scope.freevars[name] = var
        return var
    raise Undef(name)


def request_bound(scope: Scope, name: str, bound: Sym):
    if name in scope.boundvars:
        return
    scope.boundvars[name] = bound
    if scope.hold_bound:
        return
    if not scope.parent:
        raise NoAllocatorForBound(name)

    request_bound(scope.parent, name, bound)


def enter(scope: Scope, name: str) -> Sym:
    if name in scope.boundvars:
        raise BindTwice(name)
    s = scope.boundvars[name] = Sym(name, object())
    return s


def sub_scope(scope: Scope, hold_bound=False):
    return Scope({}, {}, hold_bound, scope)


class ScopedSym(namedtuple("ScopedSym", ["scope", "sym"])):
    scope: Scope
    sym: Sym


Scope.require = require
Scope.sub_scope = sub_scope
Scope.enter = enter
