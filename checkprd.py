from proud.pybackend import PyBackEnd
from argser import call
from colorama import Fore, Style
from traceback import format_exc
import os


def check_code(filename):
    end = PyBackEnd()
    # try:
    end.main(filename, filename + '.sij')


    # except Exception as e:
    #     print(e)
    #     pass

    tc = end.top
    # print('checked:', Fore.GREEN)
    # for k, v in tc.tenv.items():
    #     print(k.name, ':', tc.tc_state.infer(v))
    # print(Style.RESET_ALL)


if __name__ == '__main__':
    call(check_code)
