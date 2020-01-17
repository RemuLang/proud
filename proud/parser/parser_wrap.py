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


def get_loc(x: Token):
    return (x.lineno + 1, x.colno)


ctx['loc_'] = loc_
ctx['loc'] = get_loc
ctx['DQString'] = DQString

_parse = mk_parser(**ctx)


def _find_n(s: str, ch, n: int):
    since = 0
    for i in range(0, n - 1):
        since = s.find(ch, since)

    return s[since:s.find(ch, since)]


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
        msgs.append(f"Line {lineno + 1}, column {colno}, {msg}")

    e = SyntaxError()
    e.lineno = maxline + 1
    e.msg = '\n'.join(msgs)
    e.filename = filename
    off = token.offset
    e.offset = off
    e.text = text[:text.find('\n', off)]
    raise e
