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
        2. inst a[tau/Q] with rigid type variables, got a', and record the instantiated variables A
            -- hints for type classes: keep implicit type in a', don't separate it from a'.
        3. inst c with flexible type variables, got c', record the instantiated variables B.
            -- hints for type classes: extract out implicit types in c', separate it from c'.
        4. C = A ++ B, make a new type variable ret'
        5. unify (a', c'), unify(b[tau/Q], ret')
        6. C' = {infer(tv) | tc in C, infer(v) isa type variable} (it's a set!!)
        7. if C' is not empty:
                for each tv in ret', if tv in C', subst a distinct bound variable in,
                all those bound variables make up of Q'.
                ret' <- Q' ret'[Q'/C']
                ------------------------------------------------------------------------
                Further, for type classes, we need to update the type of arg:
                c <- Q'' c'[Q''/C'].                                       <------------
                    Why must be there some qualifiers Q''?                              |
                    Because the bound type variables of ret' is introduced by c.        |
                    -- more hints for type classes:                                     |
                    if any implicit type(I) in c', it'll get updated, too, by this step |
                    , and we should update the term arg:
                        arg <- instance(I, (term level bound variables), arg)
                        where the bound variables will be used for instance resolution.

                P.S: All type variables instantiated by Q keep mono.
                P.S: the term f can also have implicit types, but it's trivial here.
                ---------------------------------------------------------------------
           else, monotype, just return ret'
        8. GOOD NEWS: we don't have to care about the type of a', for both type infer and code gen

    let binding:
        for "let a = b [and ...] in c", it works like "f = fun a -> c, f b"
        T |- a: t1
        T |- b: t2
        procedure:
        1. instantiate t1 with rigid tvs, got t1', record the inst tvs A
        2. instantiate t2 with flexible tvs, got t2', record the inst tvs B
        3. C = A ++ B
        4. unify(t1', t2'), got env T', T' |- c : t3'
        5. C' = {infer(tv) | tc in C, infer(v) isa type variable} (it's a set!!)
        6. if C' is not empty:
              polymorphise t3' with C' (and t2', if we're to support type class)
           else just return t3'
    """
    raise NotImplementedError