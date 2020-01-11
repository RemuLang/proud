import typing as t
from collections import namedtuple, OrderedDict

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


class Sym(namedtuple("Sym", ["name", "uid", "is_cell"])):
    name: str
    uid: object
    is_cell: Ref[bool]

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

    def sub_scope(self, hold_bound=False) -> 'Scope':
        ...

    def get_newest_bounds(self) -> t.List[Sym]:
        if not self.hold_bound:
            # TODO
            raise ValueError
        names = []
        bounds = self.boundvars
        for k, v in bounds.items():
            if k in names:
                names.remove(k)
            names.append(k)

        return [bounds[n] for n in names]


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
        var.is_cell.contents = True
        return var
    raise Undef(name)


def enter(scope: Scope, name: str) -> Sym:
    if name in scope.boundvars:
        raise BindTwice(name)
    s = scope.boundvars[name] = Sym(name, object(), Ref(False))
    return s


def sub_scope(scope: Scope, hold_bound=False):
    return Scope(OrderedDict(), OrderedDict(), hold_bound, scope)


Scope.require = require
Scope.sub_scope = sub_scope
Scope.enter = enter
