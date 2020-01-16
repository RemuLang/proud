from hybridts import type_encoding as te
from hybridts.tc_state import TCState


def generalise_unify(tc_state: TCState, lhs: te.T, rhs: te.T):
    """
    function application:
            T |- f : (Q) a -> b
            T |- arg : c
            a[tau/Q] >= c
        procedure:
        1. inst ((Q) a -> b) and c
        2. inst a[tau/Q] with rigid type variables, record the instantiate map A
        3. inst arg with flexible type variables, record the instantiate map B
        4. C = {v : k for k, v in A.items()} & {v: k for k, v in B.items()}
        5. unify (a[tau/Q], arg'), unify(b[tau/Q], ret)
        6. C' = {infer(v): k for v, k in C.items() if infer(v) isa type variable}
        7. if C' is not empty:





    """
    maps = {}
    if isinstance(lhs, te.Forall):
        lhs = tc_state.inst_without_structure_preserved(lhs, )

    pass
