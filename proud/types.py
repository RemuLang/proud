from hybridts import type_encoding as te
import typing
# (a : type b) implies ((x : a) implies (x : b))
# for "value as type".
# This is not achieve in type system, but the compiler.
type_type = (te.nom_t, "type")


def type_app(f: te.T, arg: te.T):
    return te.app_t, f, arg


def nominal(typename: str):
    return te.nom_t, typename


def forall(ns: typing.FrozenSet[str], polytype: te.T):
    return te.forall_t, ns, polytype


def arrow(arg: te.T, ret: te.T):
    return te.arrow_t, arg, ret


def imply(arg: te.T, ret: te.T):
    return te.implicit_t, arg, ret


def tuple(*xs: te.T):
    return (te.tuple_t, *xs)


def record(row_t: te.Row) -> te.T:
    return te.record_t, row_t

def fresh(n: str):
    return te.fresh_t, n

bigint_t = te.nom_t, "bigint"
string_t = te.nom_t, "string"
float_t = te.nom_t, "float"
list_t = te.nom_t, "list"
complex_t = te.nom_t, "complex"


