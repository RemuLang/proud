from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
# from prettyprinter import pprint as pp, install_extras
# install_extras()
mod = parse("""
module A
export inst = 2
type my_type
let f1 = | x | -> x
export auto : forall a. a -> a = | x | -> x
let _ = f1 inst
let choose: forall a. a -> a -> a = | x | -> | y | -> x
let res = choose auto
""")
comp_ctx = CompilerCtx.top('a.prd', 'a')
modular = Modular(comp_ctx)
xs = modular.eval(mod)
tc = comp_ctx.tc_state

for k, v in comp_ctx.tenv.items():
    print(k.name, ':', tc.infer(v))

#
# def f(x):
#     if isinstance(x, ir.Expr):
#         x.type = tc.infer(x.type)
#         if isinstance(x.expr, scope.Sym):
#             print(x.expr.name, ':', x.type)
#         return
#     if isinstance(x, ir.Block):
#         for each in x.elts:
#             f(each)
#             each.pre_visit(f)
#         return
#     if isinstance(x, ir.Tuple):
#         for each in x.elts:
#             f(each)
#             each.pre_visit(f)
#         return
# xs.pre_visit(f)
