import typing as t

check_unique = set()


class ASTTag:
    def __init__(self, n, args: t.List[t.Union[str, t.Tuple[str, str]]]):
        if n in check_unique:
            raise Exception("{} has already become a ast tag".format(n))
        check_unique.add(n)
        self.name = n
        self.args = args

    def __repr__(self):
        return self.name


def_k = ASTTag("define", ["export", "gname", "type", "bound"])
let_k = ASTTag("let", ["is_rec", "name", "type", "bound", "body"])
type_k = ASTTag("type", ["name", "definition"])
and_k = ASTTag("intersect", ["left", "right"])
or_k = ASTTag("alt", ["left", "right"])
quote_k = ASTTag("quote", ["contents"])
unquote_k = ASTTag("unquote", ["contents"])
lambda_k = ASTTag("lam", ["arg", "type", "ret"])
match_k = ASTTag("match", ["target", ("cases", "builtins.list")])
ann_k = ASTTag("annotate", ["var", "type"])
bin_k = ASTTag("binary", ["head", ("tl", "Tuple[builtins.tuple, ...]")])
case_k = ASTTag("case", ["pat", "expr"])
list_k = ASTTag("list", ["elts"])
tuple_k = ASTTag("tuple", ["elts"])
record_k = ASTTag("record", ["pairs", 'row'])
pair_k = ASTTag("pair", [("key", "builtins.str"), "value"])
call_k = ASTTag("call", ["f", "arg"])
arrow_k = ASTTag("arrow", ["arg", "ret"])
imply_k = ASTTag("imply", ["arg", "ret"])
loc_k = ASTTag("loc", ["location", "contents"])
forall_k = ASTTag("forall", [("fresh_vars", "builtins.tuple"), "polytype"])
exist_k = ASTTag("exist", [("bound_vars", "builtins.tuple"), "monotype"])
guard_k = ASTTag("guard", ["expr"])
pin_k = ASTTag("pin", ["expr"])
uncall_k = ASTTag("uncall", ["f", "arg"])
cons_k = ASTTag("cons", ["branches"])
attr_k = ASTTag("attr", ["base", ("attr_name", "builtins.str")])
module_k = ASTTag("module", ['is_rec', "name", "stmts"])


def unloc(e):
    if isinstance(e, tuple) and e and e[0] is loc_k:
        a, b = unloc(e[2])
        if not a:
            return e[1], b
        return a, b
    return None, e


## Lowered
set_k = ASTTag("set", ["sym", "expr"])
inst_k = ASTTag("inst", ["type", "scope", "expr"])
typed_k = ASTTag("typed", ["type", "expr"])
func_k = ASTTag("func", ["name", "filename", "freevars", "arg", "expr"])
invoke_k = ASTTag("invoke", ["f", "arg"])
prj_k = ASTTag("prj", ["expr", "i"])
label_k = ASTTag("label", ["name"])
goto_k = ASTTag("goto", ["name"])
goto_if_k = ASTTag("goto_if", ["name", "cond"])
goto_if_not_k = ASTTag("goto_if_not", ["name", "cond"])
indir_k = ASTTag("indir", ["expr"])
addr_k = ASTTag("addr", ["name"])
block_k = ASTTag("block", ["elts"])
switch_k = ASTTag("switch", ["target", "cases", "default"])

lowered = [
    set_k, func_k, invoke_k, prj_k, label_k, goto_if_k, goto_if_not_k, goto_k,
    indir_k, addr_k, block_k, loc_k, tuple_k, list_k, switch_k, type_k, attr_k
]


def is_ast(node):
    return isinstance(node, tuple) and node and isinstance(node[0], ASTTag)
