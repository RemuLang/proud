from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
from proud import types
from hybridts import type_encoding as te
mod = parse("""
module MyMod
let test : exist a. a = coerce 0 : string 
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
