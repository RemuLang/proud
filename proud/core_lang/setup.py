from proud.core_lang import typing, expression, module

module.Express = expression.Express
module.Typing = typing.Typing
typing.Express = expression.Express
expression.Typing = typing.Typing

__all__ = []
