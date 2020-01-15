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

    def __repr__(self):
        return 'inst {!r} on {!r}'.format(self.inst_t, self.expr)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Set:
    name: scope.Sym
    expr: 'Expr'

    def __repr__(self):
        return '{} = {!r}'.format(self.name.name, self.expr)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Block:
    elts: t.List['Expr']

    def __repr__(self):
        return '\n'.join(map(repr, self.elts))


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Invoke:
    f: 'Expr'
    arg: 'Expr'

    def __repr__(self):
        return 'invoke {!r} {!r}'.format(self.f, self.arg)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Label:
    name: LabelName

    def __repr__(self):
        return 'label {}'.format(self.name.name)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Goto:
    name: LabelName

    def __repr__(self):
        return 'goto {}'.format(self.name.name)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class GotoIf:
    name: LabelName
    expr: 'Expr'

    def __repr__(self):
        return "goto {} if {}".format(self.name.name, repr(self.expr))


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class GotoIfNot:
    name: LabelName
    expr: 'Expr'

    def __repr__(self):
        return 'goto {} if not'

# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Switch:
    target: 'Expr'
    cases: t.Dict[int, LabelName]

    def __repr__(self):
        line1 = 'switch {}\n'.format(self.target)
        line2 = '\n'.join('{} : {}'.format(i, n.name) for i, n in self.cases)
        return line1 + line2
# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Fun:
    name: str
    filename: str
    args: t.List[scope.Sym]
    expr: 'Expr'

    def __repr__(self):
        line1 = "func {} at {}".format(self.name, self.filename)
        line2 = repr(self.expr)
        line3 = "end"
        return line1 + line2 + line3

# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Const:
    value: object

    def __repr__(self):
        return repr(self.value)

# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Loc:
    loc: object

    def __repr__(self):
        return "loc {}".format(self.loc)


# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class WrapLoc:
    loc: object
    expr: 'Expr'

    def __repr__(self):
        line1 = 'loc {}\n'.format(self.loc)
        line2 = repr(self.expr)
        return line1 + line2

# @derive.post_visitor(lambda _, __, s: s == 'Expr')
# @derive.pre_visitor(lambda _, __, s: s == 'Expr')
@dataclass
class Tuple:
    elts: t.List['Expr']

    def __repr__(self):
        return '({})'.format(', '.join(map(repr, self.elts)))


# # @derive.post_visitor(lambda _, __, s: s == 'Expr')
# # @derive.pre_visitor(lambda _, __, s: s == 'Expr')


@dataclass
class Coerce:
    target: 'Expr'

    def __repr__(self):
        return 'coerce {!r}'.format(self.target)


@dataclass
class Field:
    base: 'Expr'
    attr: str
    def __repr__(self):
        return '{}.{}'.format(self.base, self.attr)


@dataclass
class Merge:
    """
    record merge
    """
    left: 'Expr'
    right: 'Expr'

    def __repr__(self):
        return 'merge {} {}'.format(self.left, self.right)


BaseExpr = t.Union[Fun, Switch, Goto, GotoIf, GotoIfNot, Label, Invoke,
                   WrapLoc, Block, Set, Instance, Const, Loc, scope.Sym, Tuple,
                   Coerce, Field, Merge]


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

    def __repr__(self):
        if isinstance(self.expr, scope.Sym):
            return '({} : {})'.format(self.expr.name, self.type)
        if isinstance(self.expr, WrapLoc):
            return repr(self.expr.expr)

        return '({} : {})'.format(self.expr, self.type)