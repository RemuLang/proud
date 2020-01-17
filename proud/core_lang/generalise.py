import typing
from hybridts import type_encoding as te
from hybridts.tc_state import TCState
from proud.core_lang import types
from proud.helpers import Ref, MissingDict


def generalise(tc_state: TCState,
               TV: typing.List[te.Var],
               many: typing.List[te.T],
               loc=None,
               filename=None):
    forall = types.ForallScope(loc, filename)

    def default(_, ref=Ref(0)):
        ith = ref.contents
        ref.contents += 1
        return te.Fresh('a{}'.format(ith), forall)

    generalised_vars = MissingDict(default)
    infer = tc_state.infer
    unify = tc_state.unify

    for tv in TV:
        tv = infer(tv)
        if isinstance(tv, te.Var):
            _ = generalised_vars[tv]

    if generalised_vars:
        bounds = tuple(generalised_vars.values())
        for var, forall_bound in generalised_vars:
            unify(var, forall_bound)

        many = [te.Forall(forall, bounds, infer(each)) for each in many]
    return many
