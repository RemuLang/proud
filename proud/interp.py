from proud import sexpr
from proud import lam
from hybridts import type_encoding as te
from hybridts.tc_state import TCState
import typing as t


MALFORMED_AST = "malformed ast {}"
TOO_RICH_AST_KIND = "AST {} not supported during this phase"


class Interp:
    def __init__(self, tcs: TCState):
        self.tcs = tcs

    def top_level(self, n):
        tcs = self.tcs
        loc, n = unloc(n)
        tag = n[0]
        if tag is sexpr.let_k:
            _, ann, exp = n
            assert ann[0] is sexpr.ann_k, MALFORMED_AST.format(ann)
            _, name, ty = ann
            assert isinstance(name, str), TOO_RICH_AST_KIND.format(name)
            return lam.λ(name, ty, exp, loc)
        if tag is sexpr.type_k:
            pass
