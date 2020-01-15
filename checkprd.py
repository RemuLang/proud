from proud.parser_wrap import parse
from proud.basic_impl import Modular
from proud.core_lang import CompilerCtx
from proud.lowered_to_sexpr import resolve_type, SExprGen
from proud.codegen_lowered_sexpr import Codegen
from proud import types
from hybridts import type_encoding as te, exc
# from hybridts.tc_make import RigidStructureKeeper
from argser import call
from colorama import Fore, Style
from traceback import format_exc
import os, sys


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
    sym = comp_ctx.scope.enter("value")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.type_type)
    sym = comp_ctx.scope.enter("list")
    comp_ctx.tenv[sym] = te.App(types.type_type, types.list_t)

    modular = Modular(comp_ctx)
    err = None
    try:
        lowered_ir = modular.eval(mod)
    except exc.TypeMismatch as e:
        lowered_ir = None
        print(Fore.RED)
        print(e)
        print(Style.RESET_ALL)
        err = format_exc()

    tc = comp_ctx.tc_state
    print('checked:', Fore.GREEN)
    for k, v in comp_ctx.tenv.items():
        print(k.name, ':', tc.infer(v))
    print(Fore.YELLOW)

    for k, v in comp_ctx.tc_state.get_structures().items():
        print(k, v)
    print(Style.RESET_ALL)
    # print(repr(lowered_ir))
    if err:
        print(err)

    if lowered_ir:
        from pprint import pprint
        resolve_type(lowered_ir, comp_ctx)
        sexpr_generator = SExprGen()
        sexpr = sexpr_generator.eval(lowered_ir)
        # pprint(sexpr)
        sij_gen = Codegen(filename)
        sij_gen.eval(sexpr)
        with open(filename + '.sij', 'w') as f:
            f.write(sij_gen.feed_code())


if __name__ == '__main__':
    call(check_code)
