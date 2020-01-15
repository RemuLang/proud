from proud.parser.parser_wrap import parse
from proud.core_lang.basic_impl import Modular
from proud.core_lang.modular_compiler import CompilerCtx, types
from proud.core_lang.lowered_to_sexpr import resolve_type, SExprGen
from proud.pybackend import PyBackEnd
from hybridts import type_encoding as te, exc
# from hybridts.tc_make import RigidStructureKeeper
from argser import call
from colorama import Fore, Style
from traceback import format_exc
import os


def check_code(filename):
    end = PyBackEnd()
    end.main(filename, filename + '.sij')

    # tc = end.top
    # print('checked:', Fore.GREEN)
    # for k, v in tc.tenv.items():
    #     print(k.name, ':', tc.tc_state.infer(v))
    # print(Style.RESET_ALL)


if __name__ == '__main__':
    call(check_code)
