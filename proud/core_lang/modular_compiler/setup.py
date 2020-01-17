from proud.core_lang.modular_compiler import CompilerGlobalContext
from proud.core_lang.modular_compiler import module, expression, typing

from proud.core_lang.modular_compiler import quotation


def make_compiler_evaluators(cgc: CompilerGlobalContext):
    Modular, linkers1 = module.make(cgc)
    Express, linkers2 = expression.make(cgc)
    Typing, linkers3 = typing.make(cgc)
    Quote = quotation.make_quote(cgc, Express)
    link_src = dict(Quote=Quote, Express=Express, Typing=Typing, Modular=Modular)

    for link in linkers1:
        link(link_src)
    for link in linkers2:
        link(link_src)
    for link in linkers3:
        link(link_src)

    return Modular, Express, Typing, Quote


__all__ = ['make_compiler_evaluators']
