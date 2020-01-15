"""
names shall case no ambiguities when using this scope
"""

import typing as t
from proud.scope import Sym, Undef


class UniqueScope:
    freevars: t.Set[Sym]
    boundvars: t.Set[Sym]
    parent: t.Optional['UniqueScope']

    def __init__(self, parent: 't.Optional[UniqueScope]'):
        self.freevars = set()
        self.boundvars = set()
        self.parent = parent

    @classmethod
    def top(cls):
        return cls(None)

    def require(self, name: Sym):
        ...

    def enter(self, name: Sym):
        ...

    def shadow(self, name: Sym):
        ...

    def sub_scope(self) -> 'UniqueScope':
        ...


def require(scope: UniqueScope, name: Sym):
    assert isinstance(name, Sym), name
    if name in scope.boundvars or name in scope.freevars:
        return
    if scope.parent:
        scope.freevars.add(name)
        name.is_cell.contents = True
        require(scope.parent, name)
        return

    raise Undef(name)


def enter(scope: UniqueScope, name: Sym):
    assert isinstance(name, Sym), name
    scope.boundvars.add(name)


def sub_scope(scope: UniqueScope):
    return UniqueScope(scope)


UniqueScope.require = require
UniqueScope.sub_scope = sub_scope
UniqueScope.enter = enter
