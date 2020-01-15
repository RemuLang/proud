from proud.lowered_ir import *
from proud.core_lang import CompilerCtx
from proud.unique_scope import UniqueScope
from proud.record_layout_rearrangement import polymorphize, monomorphize
from hybridts.type_encoding import pre_visit
from contextlib import contextmanager
import typing
empty_record = te.Record(te.empty_row)
type_obj = (te.App, te.Arrow, te.Var, te.Nom, te.Fresh, te.Tuple, te.Forall,
            te.Record, te.Implicit)


class MissingDict(dict):
    def __init__(self, f):
        super().__init__()
        self.gen_key = f

    def __missing__(self, key):
        value = self[key] = self.gen_key(key)
        return value


def _sort_key(x):
    return x[0]


def replace_to_unit(_, t):
    if isinstance(t, te.Var):
        return (), empty_record
    return (), t


def resolve_type(expr: Expr, comp_ctx: CompilerCtx):
    tc_state = comp_ctx.tc_state

    def resolve(expr: Expr):
        expr_type = tc_state.infer(expr.type)
        expr_type = pre_visit(replace_to_unit)((), expr_type)
        if isinstance(expr.expr, Const):
            # TODO: serialize type objects to marshal-able repr
            expr.expr = Const(id(tc_state.infer(expr.type)))
        elif isinstance(expr.expr, Polymorphization):
            expr.type = tc_state.infer(expr.type)
        elif isinstance(expr.expr, Instance):
            expr.type = tc_state.infer(expr.type)

        expr.type = expr_type

    return visit_expr(resolve)(expr)


@contextmanager
def in_subscope(gen: 'SExprGen'):
    subscope = gen.scope.sub_scope()
    scope = gen.scope
    gen.scope = subscope
    try:
        yield subscope
    finally:
        gen.scope = scope


class SExprGen:
    def __init__(self):
        self.scope = UniqueScope.top()

    def eval(self, expr: Expr) -> tuple:
        pass

    def set(self, name, expr):
        expr = self.eval(expr)
        self.scope.enter(name)
        return sexpr.set_k, name, expr

    def block(self, elts: typing.List[Expr]):
        return sexpr.block_k, tuple(map(self.eval, elts))

    def invoke(self, f: Expr, arg: Expr):
        return sexpr.invoke_k, self.eval(f), self.eval(arg)

    @classmethod
    def label(self, name):
        return sexpr.label_k, str(name)

    def fun(self, name, filename, arg: scope.Sym, expr: Expr):

        with in_subscope(self) as sub_scope:
            sub_scope: UniqueScope
            sub_scope.enter(arg)
            expr = self.eval(expr)
            freevars = sub_scope.freevars

        return sexpr.func_k, name, filename, tuple(freevars), arg, expr

    def const(self, value):
        return value

    def wrap_loc(self, loc, expr):
        return sexpr.loc_k, loc, expr

    def tuple(self, elts):
        return sexpr.tuple_k, tuple(map(self.eval, elts))

    def coerce(self, target):
        return self.eval(target)

    def field(self, base: Expr, attr: str):
        assert isinstance(base.type, te.Record)
        fields, _ = te.extract_row(base.type.row)
        fields = sorted(fields.keys(), key=_sort_key)
        i = fields.index(attr)
        return sexpr.prj_k, (sexpr.prj_k, self.eval(base), 0), i

    def poly(self, layout_type, expr: Expr):
        layout_change = polymorphize(layout_type, expr.type)

        if not layout_change:
            assert layout_change is None
            return self.eval(expr)

        def mk_sym(i):
            sym = scope.Sym("rearrange.{}".format(i), object(), is_cell=False)
            self.scope.enter(sym)
            return sym

        sym_map = MissingDict(mk_sym)
        sym = sym_map[0]
        block = [(sexpr.set_k, sym, self.eval(expr))]

        def rec_gen(x, depth=0):
            tag = sym_map[depth]
            if not x:
                return tag
            elts = []
            if isinstance(x, tuple):
                sub = sym_map[depth + 1]
                seq = []
                for i, each in enumerate(x):
                    seq.append((sexpr.set_k, sub, (sexpr.prj_k, tag, i)))
                    seq.append(rec_gen(each, depth + 1))
                    elts.append((sexpr.block_k, seq))
            elif isinstance(x, list):
                hd, tl = x
                sub = sym_map[depth + 1]

                seq_hd = [(sexpr.set_k, sub, (sexpr.prj_k, tag, 0))]
                seq_tl = [(sexpr.set_k, sub, (sexpr.prj_k, tag, 1))]
                elt1 = []
                elt2 = []
                for i, (ith, each) in enumerate(hd):
                    elt = [(sexpr.set_k, sub, (sexpr.prj_k, tag, ith)),
                           rec_gen(each, depth + 1)]
                    elt1.append((sexpr.block_k, elt))

                for i, (ith, each) in enumerate(tl):
                    elt = [(sexpr.set_k, sub, (sexpr.prj_k, tag, ith)),
                           rec_gen(each, depth + 1)]
                    elt2.append((sexpr.block_k, elt))
                seq_hd.append(elt1)
                seq_tl.append(elt2)
                elts.extend([(sexpr.tuple_k, seq_hd), (sexpr.tuple_k, seq_tl)])
            return sexpr.tuple_k, elts

        block.append(rec_gen(layout_change))
        return sexpr.block_k, block

    def mono(self, layout_type: te.T, expr: Expr):
        layout_change = monomorphize(expr.type, layout_type)
        if not layout_change:
            return self.eval(expr)

        def mk_sym(i):
            sym = scope.Sym("rearrange.{}".format(i), object(), is_cell=False)
            self.scope.enter(sym)
            return sym

        sym_map = MissingDict(mk_sym)
        sym = sym_map[0]
        block = [(sexpr.set_k, sym, self.eval(expr))]

        def rec_gen(x, depth=0):
            tag = sym_map[depth]
            if not x:
                return tag
            elts = []
            if isinstance(x, tuple):
                sub = sym_map[depth + 1]
                seq = []
                for i, each in enumerate(x):
                    seq.append((sexpr.set_k, sub, (sexpr.prj_k, tag, i)))
                    seq.append(rec_gen(each, depth + 1))
                    elts.append((sexpr.block_k, seq))
            elif isinstance(x, list):
                hd, tl = x
                sub = sym_map[depth + 1]
                elts1 = {}
                elt2 = sexpr.tuple_k, []
                i = 0
                for i, (ith, each) in enumerate(hd):
                    elts1[ith] = [(sexpr.block_k,
                                   [(sexpr.set_k, sub,
                                     (sexpr.prj_k, (sexpr.prj_k, tag, 0), i)),
                                    rec_gen(each, depth + 1)])]
                for i, (ith, each) in enumerate(tl):
                    elts1[ith] = [(sexpr.block_k,
                                   [(sexpr.set_k, sub,
                                     (sexpr.prj_k, (sexpr.prj_k, tag, 0), i)),
                                    rec_gen(each, depth + 1)])]

                elt1 = sexpr.tuple_k, tuple(
                    e for e, _ in sorted(elts1.items(), key=_sort_key))
                elts.extend([elt1, elt2])

            return sexpr.tuple_k, elts

        block.append(rec_gen(layout_change))
        return sexpr.block_k, block

    def merge(self, left: Expr, right: Expr):
        lt, _ = te.extract_row(left.type.row)
        rt, _ = te.extract_row(left.type.row)
        l_fields = sorted(lt.keys(), key=_sort_key)
        r_fields = sorted(rt.keys(), key=_sort_key)

        all_fields = l_fields + r_fields
        all_fields.sort(key=_sort_key)
        block = []
        elt1 = []
        l_e = self.eval(left)
        r_e = self.eval(right)

        l_sym = scope.Sym("merge.left", object(), is_cell=False)
        self.scope.enter(l_sym)
        r_sym = scope.Sym("merge.right", object(), is_cell=False)
        self.scope.enter(r_sym)
        block.append((sexpr.set_k, l_sym, (sexpr.prj_k, l_e, 0)))
        block.append((sexpr.set_k, r_sym, r_e))

        for each in all_fields:
            if each in l_fields:
                i = l_fields.index(each)
                elt1.append((sexpr.prj_k, l_sym, i))
            elif each in r_fields:
                i = r_fields.index(each)
                elt1.append((sexpr.prj_k, (sexpr.prj_k, r_sym, 0), i))
        elt1 = sexpr.tuple_k, elt1
        elt2 = sexpr.prj_k, r_sym, 1
        block.append((sexpr.tuple_k, [elt1, elt2]))
        return sexpr.block_k, block