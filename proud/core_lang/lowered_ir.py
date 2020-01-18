import proud.unification.type_encode as te
import proud.core_lang.scope as scope
import typing as t
from dataclasses import dataclass
DEBUG = False


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
    scope: t.List[scope.Sym]
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
    args: scope.Sym
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
class Polymorphization:
    layout_type: te.T
    expr: 'Expr'

    def __repr__(self):
        return 'poly {} by {}'.format(self.expr, self.layout_type)


@dataclass
class Momomorphization:
    layout_type: te.T
    expr: 'Expr'

    def __repr__(self):
        return 'mono {} by {}'.format(self.expr, self.layout_type)


@dataclass
class Merge:
    """
    record merge
    """
    left: 'Expr'
    right: 'Expr'

    def __repr__(self):
        return 'merge {} {}'.format(self.left, self.right)


@dataclass
class Extern:
    foreign_code: str

    def __repr__(self):
        return 'foreign-code {}'.format(repr(self.foreign_code))


@dataclass
class ITE:
    cond: 'Expr'
    true_clause: 'Expr'
    else_clause: 'Expr'
    token = None

    def __post_init__(self):
        self.token = object()


BaseExpr = t.Union[Fun, Switch, Goto, GotoIf, GotoIfNot, Label, Invoke,
                   WrapLoc, Block, Set, Instance, Const, scope.Sym, Tuple,
                   Extern, Coerce, Field, Merge, Polymorphization, ITE,
                   Momomorphization]

LeafBaseExpr = (Goto, Label, scope.Sym, Const, Extern)


def visit_expr(action):
    def recurse_base(expr: BaseExpr) -> None:
        if isinstance(expr, LeafBaseExpr):
            return
        if isinstance(expr, ITE):
            recurse(expr.cond)
            recurse(expr.true_clause)
            recurse(expr.else_clause)
            return
        if isinstance(expr, (Polymorphization, Momomorphization)):
            return recurse(expr.expr)
        if isinstance(expr, Fun):
            return recurse(expr.expr)
        if isinstance(expr, Switch):
            return recurse(expr.target)
        if isinstance(expr, GotoIf):
            return recurse(expr.expr)
        if isinstance(expr, GotoIfNot):
            return recurse(expr.expr)
        if isinstance(expr, Invoke):
            recurse(expr.f)
            recurse(expr.arg)
            return
        if isinstance(expr, WrapLoc):
            return recurse(expr.expr)
        if isinstance(expr, (Block, Tuple)):
            for elt in expr.elts:
                recurse(elt)
            return
        if isinstance(expr, Set):
            return recurse(expr.expr)
        if isinstance(expr, Instance):
            raise NotImplementedError
        if isinstance(expr, Coerce):
            return recurse(expr.target)
        if isinstance(expr, Field):
            return recurse(expr.base)
        if isinstance(expr, Merge):
            recurse(expr.left)
            recurse(expr.right)
            return
        if isinstance(expr, str):
            return
        raise TypeError(type(expr))

    def recurse(e: Expr) -> None:
        action(e)
        expr = e.expr
        recurse_base(expr)

    return recurse


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
