from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
from proud import scope
from hybridts.tc_state import TCState
from pprint import pprint
from prettyprinter import pprint as pp, install_extras
install_extras()
mod = parse("""
module A
let x = 1
let b = 2
type a
""")
pprint(mod)

_, loc, mod = mod
comp_ctx = CompilerCtx.top('a.prd', 'a')
modular = Modular(comp_ctx)
pp(modular.loc(loc, mod))



