from proud.unification import type_encode as te

# (a : type b) implies ((x : a) implies (x : b))
# for "value as type".
# This is not achieved in type system, but the compiler.

compiler_builtin_file = object()


def show_loc(filename, loc):
    if filename is compiler_builtin_file:
        return ''
    s = ' from {}'.format(filename)
    if loc:
        s = '{}, line {}, col {}'.format(s, loc[0], loc[1])
    return s


class Nom(te.Nom):

    def __init__(self, name, loc=None, filename=compiler_builtin_file):
        self.name = name
        self.loc = loc
        self.filename = filename

    def get_name(self) -> str:
        return self.name

    def __repr__(self):
        return '{}'.format(self.name)  # show_loc(loc=self.loc, filename=self.filename))


class ForallScope(te.ForallGroup):

    def __init__(self, loc=None, filename=None):
        self.loc = loc
        self.filename = filename

    def __repr__(self):
        if self.filename is None:
            return '<forall>'

        return '<forall{}>'.format(show_loc(loc=self.loc, filename=self.filename))


class Var(te.Var):

    def __init__(self, loc, filename, name=None, is_rigid=False):
        self.is_rigid = is_rigid
        self.name = name or hex(id(self))
        self.loc = loc
        self.filename = filename

    def __repr__(self):
        return '\'{}:{}'.format(self.name, self.level)


def fresh(n: str):
    return te.UnboundFresh(n)


type_type = Nom("type")
anyway_type = Nom("anyway(shouldn't be awared by users!)")
int_t = Nom("int")
string_t = Nom("string")
float_t = Nom("float")
list_t = Nom("list")
complex_t = Nom("complex")
bool_t = Nom("bool")
unit_t = Nom("unit")

_bot_forall_scope = ForallScope()
_bot_bound = te.Bound("a", _bot_forall_scope)
bot = te.Forall(_bot_forall_scope, (_bot_bound,), _bot_bound)
