from proud.unification.type_encode import *
from proud.unification.tc_make import make
from contextlib import contextmanager
import typing as t


class TCState:
    def __init__(self):
        make(self)

    def push_level(self) -> Level:
        raise NotImplementedError

    def new_var(self, *props, Var=None, **kwargs) -> Var:
        raise NotImplementedError

    def user_var(self, *props, Var=None, **kwargs) -> Var:
        """
        create a tv, which can be treated as a closed one
        for all levels
        """
        raise NotImplementedError

    def occur_in(self, var: T, ty: T) -> bool:
        raise NotImplementedError

    def inst(self, maybepoly: T, rigid=False) -> t.Tuple[t.Dict[T, Var], T]:
        raise NotImplementedError

    def infer(self, ty: T) -> T:
        raise NotImplementedError

    def unify(self, lhs: T, rhs: T) -> None:
        raise NotImplementedError

    def extract_row(self, rowt: Row) -> t.Optional[t.Dict[str, T]]:
        raise NotImplementedError
