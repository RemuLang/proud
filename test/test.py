from proud.parser_wrap import parse
from pprint import pprint
pprint(parse("""
let x = 1
let b = 2
type a
let f = | a or b | => 1
"""))