from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
# from prettyprinter import pprint as pp, install_extras
# install_extras()
mod = parse("""
module MyMod
let x : forall a. a -> exist hola. hola = coerce 0
let restrain: forall b. b -> b = coerce 0
let z : exist a. a -> a = coerce 0
let check1 = restrain x
let check2 = restrain restrain 
let check3 = restrain z
let res2 : exist a. a -> a = coerce 0
let check4 = x res2
""")
comp_ctx = CompilerCtx.top('a.prd', 'a')
modular = Modular(comp_ctx)
xs = modular.eval(mod)
tc = comp_ctx.tc_state

for k, v in comp_ctx.tenv.items():
    print(k.name, ':', tc.infer(v), '============', v)

