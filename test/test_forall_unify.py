from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
# from prettyprinter import pprint as pp, install_extras
# install_extras()
mod = parse("""
module MyMod
let mid: forall b. b = coerce 0
let x : forall a. a -> a = mid


""")
comp_ctx = CompilerCtx.top('a.prd', 'a')
modular = Modular(comp_ctx)
xs = modular.eval(mod)
tc = comp_ctx.tc_state

for k, v in comp_ctx.tenv.items():
    print(k.name, ':', tc.infer(v), '============', v)

