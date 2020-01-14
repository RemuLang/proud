from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
from proud import types
from hybridts import type_encoding as te
# from prettyprinter import pprint as pp, install_extras
# install_extras()
mod = parse("""
module MyMod
let add : bigint -> bigint -> bigint = coerce 0
let y : {c: string} = coerce 0
let x = {a = 0, b = 3 | y}
""")
comp_ctx = CompilerCtx.top('a.prd', 'a')
sym = comp_ctx.scope.enter("bigint")
comp_ctx.tenv[sym] = te.App(types.type_type, types.bigint_t)
sym = comp_ctx.scope.enter("string")
comp_ctx.tenv[sym] = te.App(types.type_type, types.string_t)

modular = Modular(comp_ctx)

xs = modular.eval(mod)
tc = comp_ctx.tc_state

for k, v in comp_ctx.tenv.items():
    print(k.name, ':', tc.infer(v), '============', v)
