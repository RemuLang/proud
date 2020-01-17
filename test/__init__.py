from proud.unification.interface import TCState

import proud.unification.type_encode as te

cnt = 0
tcs = TCState()

list = te.InternalNom("list")
var = tcs.new_var()

t1 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.UnboundFresh("x")))
t2 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), var))

tcs.unify(t1, t2)
# print(tcs.infer(t2))

x1 = tcs.new_var()
x2 = tcs.new_var()

int_t = te.InternalNom("base.int")

tcs.unify(x1, int_t)
tcs.unify(x1, x2)

assert tcs.infer(x1) == int_t
assert tcs.infer(x2) == int_t

x3 = tcs.new_var()
r1 = te.row_of_map({'a': x1, 'b': x3}, te.empty_row)
r1 = te.Record(r1)
tho = tcs.new_var()
r2 = te.row_of_map({'a': x3}, te.RowPoly(tho))
r2 = te.Record(r2)
tcs.unify(r1, r2)
# print(tcs.infer(r1))
# print(tcs.infer(r2))
# print(tcs.infer(tho))

var = tcs.new_var()
var2 = tcs.new_var()

t1 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.UnboundFresh("x")))
t2 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), var))

tcs.unify(te.App(var, var), var2)
try:
    tcs.unify(t1, t2)
except te.excs.TypeMismatch:
    pass

var = tcs.new_var()
var2 = tcs.new_var()

t1 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.Arrow(te.UnboundFresh("x"), var)))
t2 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.Arrow(var2, var2)))
tcs.unify(var, var2)
tcs.unify(t1, t2)
print(tcs.infer(var))
print(var.belong_to.vars)


var = tcs.new_var()
var2 = tcs.new_var()

t1 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.Arrow(var, te.UnboundFresh("x"))))
t2 = te.normalize_forall(["x"], te.App(te.UnboundFresh("x"), te.Arrow(var2, var2)))
tcs.unify(t1, t2)
print(tcs.infer(var))
print(var.belong_to.vars)
