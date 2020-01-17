class DuplicatedNamedTypeVar(Exception):

    def __init__(self, vars):
        self.vars = vars


class InvalidListType(Exception):

    def __init__(self, t):
        self.t = t


class StaticCheckingFailed(Exception):
    exc: Exception
    clc: object

    def __init__(self, exc, clc):
        self.args = (exc, clc)
        self.exc = exc
        self.clc = clc


class TypeCheckError(Exception):
    pass


class IllFormedType(TypeCheckError):
    form: str

    def __init__(self, form):
        self.form = form


class RowFieldDuplicated(TypeCheckError):

    def __init__(self, field: str):
        self.field = field


class TypeMismatch(TypeCheckError):
    lhs: object
    rhs: object

    def __init__(self, lhs, rhs):
        self.lhs = lhs
        self.rhs = rhs


class ForallVarExceedScope(TypeCheckError):
    forall_scope: object
    var: object

    def __init__(self, scope, var):
        self.forall_scope = scope
        self.var = var


class ShouldNotFreshVarHere(TypeCheckError):
    """
    This might be due to you're instantiating a forall type which contains
    multiple bound variables and at least 1 free variable.
    """


class RigidTypeExpanding(TypeCheckError):
    var: object

    def __init__(self, var: object):
        self.var = var


class StructureCannotUnify(TypeCheckError):
    pass


class RowCheckFailed(Exception):
    pass
