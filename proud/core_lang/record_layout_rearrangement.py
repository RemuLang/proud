from proud.unification import type_encode as te


def _sort_key(x):
    return x[0]


def check_normalized(seq):
    if not seq:
        return True
    n = len(seq)
    for i in range(n):
        if seq[i] != i:
            return False
    return True


def _unforall(x: te.Forall):
    while isinstance(x, te.Forall):
        x = x.poly_type
    return x


def rec_polymorphize(require: te.T, actual: te.T):
    """
    Layout := Tuple (Layout, ...)
           |  List  [(Int, Layout), ..., (Int, ...)]
           |  None

    """
    if isinstance(require, te.Tuple):
        assert isinstance(actual, te.Tuple)

        ret = tuple(rec_polymorphize(req, act) for req, act in zip(require.elts, actual.elts))
        if all(e is None for e in ret):
            return None
        return ret
    if isinstance(actual, te.Record):
        assert isinstance(actual, te.Record)
        require_field_ts, _ = te.extract_row(require.row)
        actual_field_ts, _ = te.extract_row(actual.row)
        require_fields = sorted(require_field_ts.keys(), key=_sort_key)
        actual_fields = sorted(actual_field_ts.keys(), key=_sort_key)
        actual_fields = {n: i for i, n in enumerate(actual_fields)}
        ret = []
        for i, f in enumerate(require_fields):
            ret.append((actual_fields.pop(f), rec_polymorphize(require_field_ts[f], actual_field_ts[f])))
        if check_normalized(ret) and not actual_fields:
            # final[i] = have[ret[i]]
            return None

        tl = tuple(i for _, i in sorted(actual_fields.items(), key=lambda x: x[1]))

        return [tuple(ret), tl]
    return None


def polymorphize(require: te.T, actual: te.T):
    require = _unforall(require)
    actual = _unforall(actual)
    if not isinstance(require, (te.Tuple, te.Record)):
        return
    if not isinstance(actual, (te.Tuple, te.Record)):
        return
    return rec_polymorphize(require, actual)


def rec_monomorphize(require: te.T, actual: te.T):
    if isinstance(actual, te.Tuple):
        assert isinstance(require, te.Tuple)
        ret = tuple(rec_monomorphize(req, act) for req, act in zip(require.elts, actual.elts))
        if all(e is None for e in ret):
            return
        return ret
    if isinstance(actual, te.Record):
        assert isinstance(require, te.Record)
        require_field_ts, _ = te.extract_row(require.row)
        actual_field_ts, _ = te.extract_row(actual.row)
        actual_fields = sorted(require_field_ts.keys(), key=_sort_key)
        require_fields = sorted(actual_field_ts.keys(), key=_sort_key)
        require_fields = {n: i for i, n in enumerate(require_fields)}

        hd = []
        for i, f in enumerate(actual_fields):
            # final[ret[i]] = have[i]
            hd.append((require_fields.pop(f), rec_monomorphize(require_field_ts[f], actual_field_ts[f])))
        if check_normalized(hd) and not require_fields:
            return None

        tl = tuple(i for _, i in sorted(require_fields, key=lambda x: x[1]))
        return [tuple(hd), tl]

    return None


def monomorphize(require: te.T, actual: te.T):
    require = _unforall(require)
    actual = _unforall(actual)
    if not isinstance(require, (te.Tuple, te.Record)):
        return
    if not isinstance(actual, (te.Tuple, te.Record)):
        return

    return rec_monomorphize(require, actual)
