from proud.core_lang.modular_compiler import Evaluator, CompilerCtx
from typing_extensions import Protocol
import typing


class BackEnd:
    def codegen(self, compiler_ctx: CompilerCtx) -> Evaluator:
        raise NotImplementedError

    def init_compiler_ctx(self) -> CompilerCtx:
        raise NotImplementedError

    def search_module(self,
                      qualname: typing.List[str]) -> typing.Callable[[], str]:
        """
        Given a qualified name of a module, return a thunk which will return the source code
        """
        raise NotImplementedError
