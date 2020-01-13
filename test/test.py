from proud.parser_wrap import parse
from pprint import pprint
pprint(parse("""
module A
let x = 1
let b = 2
type a
"""))