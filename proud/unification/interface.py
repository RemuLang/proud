from proud.unification.type_encode import *
from proud.unification.tc_make import make
import typing as t


class TCState:
    def __init__(self):
        make(self, [set()])

    def push_level(self):
        levels = self.levels()
        new = set()
        levels.append(new)
        return new

    def pop_level(self):
        return self.levels().pop()

    def levels(self) -> t.List[t.Set[Var]]:
        raise NotImplementedError

    def new_var(self, *props, Var=None, **kwargs) -> Var:
        raise NotImplementedError

    def occur_in(self, var: T, ty: T) -> bool:
        raise NotImplementedError

    def inst(self, maybepoly: T) -> t.Tuple[t.Dict[T, Var], T]:
        raise NotImplementedError

    def infer(self, ty: T) -> T:
        raise NotImplementedError

    def unify(self, lhs: T, rhs: T) -> None:
        raise NotImplementedError

    def extract_row(self, rowt: Row) -> t.Optional[t.Dict[str, T]]:
        raise NotImplementedError
