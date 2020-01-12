class DuplicatedNamedTypeVar(Exception):
    def __init__(self, vars):
        self.vars = vars


class InvalidListType(Exception):
    def __init__(self, t):
        self.t = t
