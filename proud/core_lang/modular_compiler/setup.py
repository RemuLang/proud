from proud.core_lang.modular_compiler import module, expression, typing

from proud.core_lang.modular_compiler import quotation

module.Express = expression.Express
module.Typing = typing.Typing
typing.Express = expression.Express
expression.Typing = typing.Typing
expression.Quote = quotation.Quote

__all__ = []
