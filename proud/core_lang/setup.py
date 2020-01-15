from proud.core_lang import typing, expression, module
from proud.core_lang import quotation

module.Express = expression.Express
module.Typing = typing.Typing
typing.Express = expression.Express
expression.Typing = typing.Typing
expression.Quote = quotation.Quote


quotation

__all__ = []
