import hybridts.type_encoding as te
import proud.scope as scope
from proud import derive
from dataclasses import dataclass
import typing as t


class LabelName:
    __slots__ = ['name']

    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return '<label {}>'.format(self.name)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Instance:
    inst_t: te.T
    scope: scope.Scope
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Set:
    name: scope.Sym
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Block:
    elts: t.List['Expr']


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Invoke:
    f: 'Expr'
    arg: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Project:
    base: 'Expr'
    item: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Label:
    name: LabelName


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Goto:
    name: LabelName


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class GotoIf:
    name: LabelName
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class GotoIfNot:
    name: LabelName
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Switch:
    target: 'Expr'
    cases: t.Dict[int, LabelName]


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Fun:
    name: str
    filename: str
    freevars: t.List[scope.Sym]
    args: t.List[scope.Sym]
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Const:
    value: object


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Loc:
    loc: object


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class WrapLoc:
    loc: object
    expr: 'Expr'


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Tuple:
    elts: t.List['Expr']

# # @derive.post_visitor(lambda _, __, s: s == 'Expr')
# # @derive.pre_visitor(lambda _, __, s: s == 'Expr')

@dataclass
class Coerce:
    target: 'Expr'


BaseExpr = t.Union[Fun, Switch, Goto, GotoIf, GotoIfNot, Label, Project,
                   Invoke, WrapLoc, Block, Set, Instance, Const, Loc,
                   scope.Sym, Tuple, Coerce]


# @derive.post_visitor(lambda _, __, s: s == BaseExpr)
# @derive.pre_visitor(lambda _, __, s: s == BaseExpr)
@dataclass
class Expr:
    type: te.T
    expr: BaseExpr

    def post_visit(self, f) -> None:
        raise NotImplementedError

    def pre_visit(self, f) -> None:
        raise NotImplementedError


