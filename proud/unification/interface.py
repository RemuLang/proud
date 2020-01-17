from hybridts.type_encoding import *
from hybridts import tc_make
import typing as t


class TCState:
    def __init__(self, tctx: TypeCtx):
        tc_make.make(self, tctx)

    def get_tctx(self):
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

    eq_fresh: t.Dict[Bound, Bound]

    def copy(self):
        tcs = TCState(self.get_tctx().copy())
        return tcs
