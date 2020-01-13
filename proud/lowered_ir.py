import hybridts.type_encoding as te
import proud.scope as scope
import typing as t
from dataclasses import dataclass


class LabelName:
    __slots__ = ['name']

    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return '<label {}>'.format(self.name)


@dataclass
class Instance:
    inst_t: te.T
    scope: scope.Scope
    expr: 'Expr'


@dataclass
class Set:
    name: scope.Sym
    expr: 'Expr'


@dataclass
class Block:
    elts: t.List['Expr']


@dataclass
class Invoke:
    f: 'Expr'
    arg: 'Expr'


@dataclass
class Project:
    base: 'Expr'
    item: 'Expr'


@dataclass
class Label:
    name: LabelName


@dataclass
class Goto:
    name: LabelName


@dataclass
class GotoIf:
    name: LabelName
    expr: 'Expr'


@dataclass
class GotoIfNot:
    name: LabelName
    expr: 'Expr'


@dataclass
class Switch:
    target: 'Expr'
    cases: t.Dict[int, LabelName]


@dataclass
class Fun:
    name: str
    filename: str
    freevars: t.List[scope.Sym]
    args: t.List[scope.Sym]
    expr: 'Expr'


@dataclass
class Const:
    value: object

@dataclass
class Loc:
    loc: object

@dataclass
class WrapLoc:
    loc: object
    expr: 'Expr'

@dataclass
class Tuple:
    elts: t.List['Expr']

BaseExpr = t.Union[Fun, Switch, Goto, GotoIf, GotoIfNot, Label, Project,
                   Invoke, WrapLoc, Block, Set, Instance, Const, Loc,
                   scope.Sym, Tuple]



@dataclass
class Expr:
    type: te.T
    expr: BaseExpr
