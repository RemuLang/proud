from hybridts import type_encoding as te
import typing
# (a : type b) implies ((x : a) implies (x : b))
# for "value as type".
# This is not achieve in type system, but the compiler.


compiler_builtin_file = object()

def show_loc(filename, loc):
    if filename is compiler_builtin_file:
        return ''
    s = 'from {}'.format(filename)
    if loc:
        s = '{}, line {}, col {}'.format(s, loc[0], loc[1])
    return s




class Nom(te.Nom):
    def __init__(self, name, loc=None, filename=None):
        self.name = name
        self.loc = loc
        self.filename = filename

    def get_name(self) -> str:
        return self.name

    def __repr__(self):

        return '<{} {}>'.format(self.name, show_loc(loc=self.loc, filename=self.filename))


class ForallScope(te.ForallGroup):
    def __init__(self, loc, filename):
        self.loc = loc
        self.filename = filename

    def __repr__(self):
        return '<forall {}>'.format(show_loc(loc=self.loc, filename=self.filename))


class Var(te.Var):
    def __init__(self, loc, filename, name=None, is_rigid=False):
        self.is_rigid = is_rigid
        self.name = name or hex(id(self))
        self.loc = loc
        self.filename = filename

    def __repr__(self):
        return '<{} {}>'.format(self.name,
                                show_loc(filename=self.filename, loc=self.loc))


def forall(ns: typing.FrozenSet[str], polytype: te.T, loc=None, filename=None):
    loc = loc or '<unknown>'
    filename = filename or '<unknown>'
    return te.normalize_forall(ForallScope(loc, filename), ns, polytype)


def fresh(n: str):
    return te.UnboundFresh(n)


type_type = Nom("value")

bigint_t = Nom("bigint")
string_t = Nom("string")
float_t = Nom("float")
list_t = Nom("list")
complex_t = Nom("complex")
unit_t = Nom("unit")
