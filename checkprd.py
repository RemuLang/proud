from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx
from proud import types
from hybridts import type_encoding as te
from argser import call
from colorama import Fore, Style
import os


def check_code(filename):
    print(Fore.YELLOW, end='')
    print('filename: ', filename)
    path = os.path.split(filename)[0]
    with open(filename) as f:
        code = f.read()
    print('code:')
    print(Fore.BLUE, end='')
    print(code, Style.RESET_ALL)

    mod = parse(code)
    comp_ctx = CompilerCtx.top(filename, path)
    sym = comp_ctx.scope.enter("bigint")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.bigint_t)
    sym = comp_ctx.scope.enter("string")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.string_t)
    sym = comp_ctx.scope.enter("bool")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.bool_t)
    sym = comp_ctx.scope.enter("float")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.float_t)
    sym = comp_ctx.scope.enter("unit")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.unit_t)

    modular = Modular(comp_ctx)
    lowered_ir = modular.eval(mod)
    tc = comp_ctx.tc_state
    print('checked:', Fore.GREEN)
    for k, v in comp_ctx.tenv.items():
        print(k.name, ':', tc.infer(v))
    print(Style.RESET_ALL)


call(check_code)
