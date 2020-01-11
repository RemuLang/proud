from proud.parser_wrap import parse
from pprint import pprint
pprint(parse("""
let x = 1
let b = 2
type a
let rec f = 1 + a * c / 3
"""))