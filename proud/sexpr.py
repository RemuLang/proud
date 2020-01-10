check_unique = set()


class ASTTag:
    def __init__(self, n):
        if n in check_unique:
            raise Exception("{} has already become a ast tag".format(n))
        check_unique.add(n)
        self.n = n

    def __eq__(self, other):
        return self is other

    def __repr__(self):
        return self.n

stmt_k = ASTTag("stmt")
expr_k = ASTTag("expr")
let_k = ASTTag("let")
type_k = ASTTag("type")
and_k = ASTTag("and")
or_k = ASTTag("or")
quote_k = ASTTag("quote")
unquote_k = ASTTag("unquote")
lambda_k = ASTTag("lambda")
match_k = ASTTag("match")
ann_k = ASTTag("ann")
bin_k = ASTTag("bin")
case_k = ASTTag("case")
list_k = ASTTag("list")
tuple_k = ASTTag("tuple")
record_k = ASTTag("record")
pair_k = ASTTag("pair")
call_k = ASTTag("call")
arrow_k = ASTTag("arrow")
imply_k = ASTTag("imply")
loc_k = ASTTag("loc")
forall_k = ASTTag("forall")
guard_k = ASTTag("guard")
pin_k = ASTTag("pin")
uncall_k = ASTTag("uncall")
alias_k = ASTTag("alias")
attr_k = ASTTag("attr")

