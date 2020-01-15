from proud.parser.parser_gen import *
from proud.core_lang.sexpr import loc_k
from proud.core_lang import sexpr
from rbnf_rts.rts import Tokens, State
from rbnf_rts.token import Token
from rbnf_rts.routine import DQString

__all__ = ['parse']
co = mk_parser.__code__
requires = co.co_varnames[:co.co_argcount]
ctx = {}

for each in requires:
    ctx[each] = getattr(sexpr, each, None)


def loc_(a, x: Token):
    return loc_k, (x.lineno + 1, x.colno), a


ctx['loc_'] = loc_
ctx['DQString'] = DQString


_parse = mk_parser(**ctx)


def parse(text: str, filename: str = "unknown"):
    tokens = list(run_lexer(filename, text))
    res = _parse(State(), Tokens(tokens))
    if res[0]:
        return res[1]
    msgs = []
    assert res[1]
    maxline = 0
    for each in res[1]:
        i, msg = each
        token = tokens[i]
        lineno = token.lineno
        maxline = max(lineno, maxline)
        colno = token.colno
        msgs.append(f"Line {lineno}, column {colno}, {msg}")

    e = SyntaxError()
    e.lineno = maxline + 1
    e.msg = '\n'.join(msgs)
    e.filename = filename
    e.offset = token.offset
    e.text = text
    raise e
