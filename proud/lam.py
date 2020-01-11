import abc


class Lam(abc.ABC):
    @abc.abstractmethod
    def λ(self, arg, type, body, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def let(self, is_rec, name, bound, body, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def var(self, name, type, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def app(self, f, arg, type, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def val(self, lit, type, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def type_app(self, f, arg, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def type_var(self, name: str, loc):
        raise NotImplementedError

    @abc.abstractmethod
    def type_tuple(self, xs, loc):
        raise NotImplementedError


def λ(arg, type, body, loc):
    return lambda lam: lam.var(arg, type(lam), body(lam), loc)


def var(name, type, loc):
    return lambda lam: lam.var(name, type(lam), loc)


def app(f, arg, type, loc):
    return lambda lam: lam.app(f, arg, type, loc)


def val(lit, type, loc):
    return lambda lam: lam.val(lit, type, loc)


def let(is_rec, name, bound, body, loc):
    return lambda lam: lam.val(is_rec, name, bound, body, loc)
