import typing as t
from collections import namedtuple, OrderedDict
from proud import derive

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


@derive.pre_visitor(lambda _, __, ___: False)
@derive.post_visitor(lambda _, __, ___: False)
class Sym(namedtuple("Sym", ["name", "uid", "is_cell"])):
    name: str
    uid: object
    is_cell: Ref[bool]


class Scope:
    freevars: t.Dict[str, Sym]
    boundvars: t.Dict[str, Sym]
    hold_bound: bool
    parent: t.Optional['Scope']
    allow_external: bool

    def __init__(self,
                 hold_bound: bool,
                 parent: 't.Optional[Scope]',
                 *,
                 allow_external: bool = False):
        self.freevars = {}
        self.boundvars = {}
        self.hold_bound = hold_bound
        self.parent = parent
        self.allow_external = allow_external

    @classmethod
    def top(cls):
        return cls(True, None)

    @classmethod
    def top_support_external(cls):
        return cls(True, None, allow_external=True)

    def require(self, name: str) -> Sym:
        ...

    def enter(self, name: str) -> Sym:
        ...

    def shadow(self, name: str) -> Sym:
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
    assert isinstance(name, str), name
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
    # external
    if scope.allow_external:
        var = Sym(name, object(), Ref(False))
        scope.freevars[name] = var
        return var
    raise Undef(name)


def enter(scope: Scope, name: str) -> Sym:
    assert isinstance(name, str), name
    if name in scope.boundvars:
        raise BindTwice(name)
    s = scope.boundvars[name] = Sym(name, object(), Ref(False))
    return s


def shadow(scope: Scope, name: str) -> Sym:
    assert isinstance(name, str), name
    s = scope.boundvars[name] = Sym(name, object(), Ref(False))
    return s


def sub_scope(scope: Scope, hold_bound=False):
    return Scope(hold_bound, scope)


Scope.require = require
Scope.sub_scope = sub_scope
Scope.enter = enter
Scope.shadow = shadow
