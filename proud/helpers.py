import typing
T = typing.TypeVar('T')


class Ref(typing.Generic[T]):
    contents: T

    def __init__(self, v: T):
        self.contents = v


class MissingDict(dict):

    def __init__(self, f):
        super().__init__()
        self.gen_value = f

    def __missing__(self, key):
        value = self[key] = self.gen_value(key)
        return value
