from proud.unification.type_encode import *
from proud import excs
import typing as t

try:
    # noinspection PyUnresolvedReferences
    from proud.unification.tc_state import TCState
except ImportError:
    pass


def make(self: 'TCState', levels: t.List[t.Set[Var]]):
    def new_var(*props, Var=None, **kwargs):
        group = PropertyTVGroup(*props)
        level = len(levels) - 1
        assert level >= 0
        var = (Var or InternalVar)(level, **kwargs)
        var.belong_to = group
        group.vars.add(var)
        levels[level].add(var)
        return var

    def infer_row(x: Row) -> Row:
        if isinstance(x, RowCons):
            return RowCons(x.field_name, infer(x.field_type), infer_row(x.tail))
        if isinstance(x, RowMono):
            return x
        if isinstance(x, RowPoly):
            return RowPoly(infer(x.type))
        raise TypeError(x)

    def infer(x: T) -> T:
        if isinstance(x, Nom):
            return x
        if isinstance(x, GenericVar):
            if x.eq:
                return x.eq
            return x
        if isinstance(x, Var):
            belong = x.belong_to
            y = belong.final
            if not y:
                return x
            assert not isinstance(y, Var)
            y = belong.final = infer(y)
            return y
        if isinstance(x, Bound):
            return x
        if isinstance(x, App):
            return App(infer(x.f), infer(x.arg))
        if isinstance(x, Arrow):
            return Arrow(infer(x.arg), infer(x.ret))
        if isinstance(x, Implicit):
            return Implicit(infer(x.witness), infer(x.type))
        if isinstance(x, Tuple):
            return Tuple(tuple(map(infer, x.elts)))
        if isinstance(x, Forall):
            return Forall(x.token, x.fresh_vars, infer(x.poly_type))
        if isinstance(x, Record):
            row = infer_row(x.row)
            if isinstance(row, RowPoly):
                return row.type
            return Record(row)
        raise TypeError(x)

    def inst_forall(bound_vars: t.Iterable[Bound], polytype: T):
        mapping: t.Dict[T, Var] = {b: GenericVar(b.name) for b in bound_vars}
        _, monotype = fresh_bounds(polytype, mapping)
        return mapping, monotype

    def inst(type, rigid=True) -> t.Tuple[t.Dict[T, Var], T]:
        """
        When using this, there should be no free variable in the scope of forall!
        """
        if isinstance(type, Forall):
            if rigid:
                mapping = {b: new_var(is_rigid) for b in type.fresh_vars}
            else:
                mapping = {b: new_var() for b in type.fresh_vars}
            type = type.poly_type
            _, type = fresh_bounds(type, mapping)
            return mapping, type
        return {}, type

    def _unify(lhs: T, rhs: T) -> None:
        if lhs is rhs:
            # Nom, Fresh, Var
            return

        if isinstance(lhs, Var):
            if occur_in(lhs, rhs):
                raise excs.IllFormedType(" a = a -> b")
            lhs.belong_to.final_to(rhs)
            return

        if isinstance(rhs, Var):
            return _unify(rhs, lhs)

        if isinstance(lhs, GenericVar) and isinstance(rhs, GenericVar):
            if not lhs.eq and not rhs.eq:
                lhs.eq = rhs
                rhs.eq = rhs
                return
            elif lhs.eq and rhs.eq and ((lhs.eq is rhs and rhs.eq is rhs) or (rhs.eq is lhs and lhs.eq is lhs)):
                return
            else:
                raise excs.TypeMismatch(lhs, rhs)

        if isinstance(lhs, Forall) and isinstance(rhs, Forall):
            ft_l = ftv(lhs.poly_type)
            ft_r = ftv(rhs.poly_type)
            freshes_l, l_p = inst_forall(lhs.fresh_vars, lhs.poly_type)
            freshes_r, r_p = inst_forall(rhs.fresh_vars, rhs.poly_type)

            unify(l_p, r_p)
            xs = {}

            def _mismatch():
                raise excs.TypeMismatch(infer(lhs), infer(rhs))

            for k, v in freshes_l.items():
                v = infer(v)
                if not isinstance(v, GenericVar):
                    _mismatch()
                if v in xs:
                    _mismatch()
                xs[v] = k

            ys = {}
            for k, v in freshes_r.items():
                v = infer(v)
                if not isinstance(v, GenericVar):
                    _mismatch()
                if v not in xs:
                    _mismatch()
                ys[v] = (xs.pop(v), k)

            if xs:
                _mismatch()

            l = {k: v[0] for k, v in ys.items()}
            r = {k: v[1] for k, v in ys.items()}

            ft_l = [(each, infer(each)) for each in ft_l]
            ft_r = [(each, infer(each)) for each in ft_r]

            for orig, pruned in ft_l:
                new = subst(l, pruned)
                if orig.belong_to.link_from or not visit_check(lambda x: not isinstance(x, GenericVar))(new):
                    raise _mismatch()
                orig.belong_to.final_to(new)

            for orig, pruned in ft_r:
                new = subst(r, pruned)

                if orig.belong_to.link_from:
                    _mismatch()
                if not visit_check(lambda x: not isinstance(x, GenericVar))(new):
                    _mismatch()

                orig.belong_to.final_to(new)

            return

        if isinstance(lhs, Implicit) and isinstance(rhs, Implicit):
            unify(lhs.witness, rhs.witness)
            unify(lhs.type, rhs.type)
            return

        if isinstance(lhs, Bound) and isinstance(rhs, Bound):
            if lhs.token is rhs.token and lhs.name == rhs.name:
                return
            raise excs.TypeMismatch(lhs, rhs)
        if isinstance(lhs, Arrow) and isinstance(rhs, Arrow):
            unify(lhs.arg, rhs.arg)
            unify(lhs.ret, rhs.ret)
            return

        if isinstance(lhs, App) and isinstance(rhs, App):
            unify(lhs.f, rhs.f)
            unify(lhs.arg, rhs.arg)
            return

        if isinstance(lhs, Tuple) and isinstance(rhs, Tuple):
            for a, b in zip(lhs.elts, rhs.elts):
                unify(a, b)
            return

        if isinstance(lhs, Record) and isinstance(rhs, Record):
            m1, ex1 = extract_row(lhs.row)
            m2, ex2 = extract_row(rhs.row)
            common_keys = m1.keys() & m2.keys()
            only_by1 = {k: v for k, v in m1.items() if k not in common_keys}
            only_by2 = {k: v for k, v in m2.items() if k not in common_keys}

            for k in common_keys:
                unify(m1[k], m2[k])

            def row_check(row1, row2, only_by1, only_by2):
                if row1 is None and row2 is None:
                    if only_by1 or only_by2:
                        raise excs.RowCheckFailed
                    return
                if row2 is None:
                    return row_check(row2, row1, only_by2, only_by1)
                if row1 is None:
                    # only_by1 == {only_by2 | row2}
                    #  where
                    #    only_by1 \cap only_by2 = \emptyset,
                    #  therefore,
                    #    only_by2 = \emptyset,
                    #    row2 = only_by1
                    if only_by2:
                        raise excs.RowCheckFailed
                    return unify(row2, Record(row_of_map(only_by1, empty_row)))
                # {only_by1|row1} == {only_by2|row2},
                # where
                #   only_by1 \cap only_by2 = \emptyset,
                # therefore,
                #   forall type in only_by2. type in row1, and
                #   forall type in only_by1. type in row2,
                # therefore,
                #   t1 = {only_by1 \cup only_by2|row} = t2,
                #   {only_by1|row} = row2
                #   {only_by2|row} = row1
                polyrow = RowPoly(new_var())
                ex2 = Record(row_of_map(only_by1, polyrow))
                ex1 = Record(row_of_map(only_by2, polyrow))
                unify(row1, ex1)
                unify(row2, ex2)

            try:
                return row_check(ex1, ex2, only_by1, only_by2)
            except excs.RowCheckFailed:
                raise excs.TypeMismatch(lhs, rhs)
        raise excs.TypeMismatch(lhs, rhs)

    def unify(lhs, rhs):
        lhs = infer(lhs)
        rhs = infer(rhs)
        _unify(lhs, rhs)

    self.inst = inst
    self.unify = unify
    self.occur_in = occur_in
    self.extract_row = extract_row
    self.infer = infer
    self.new_var = new_var
