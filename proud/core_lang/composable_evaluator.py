import builtins
import abc
from typing import Tuple
from proud.core_lang.sexpr import *
from typing_extensions import Protocol
dispatcher = {}


class Eval_literal(Protocol):
    @abc.abstractmethod
    def literal(module, val):
        ...


def literal(val):
    return lambda module: module.literal(val)


dispatcher[lit_k] = literal


class Eval_coerce(Protocol):
    @abc.abstractmethod
    def coerce(module, expr):
        ...


def coerce(expr):
    return lambda module: module.coerce(expr)


dispatcher[coerce_k] = coerce


class Eval_define(Protocol):
    @abc.abstractmethod
    def define(module, export, gname, type, bound):
        ...


def define(export, gname, type, bound):
    return lambda module: module.define(export, gname, type, bound)


dispatcher[def_k] = define


class Eval_let(Protocol):
    @abc.abstractmethod
    def let(module, is_rec, seq, body):
        ...


def let(is_rec, seq, body):
    return lambda module: module.let(is_rec, seq, body)


dispatcher[let_k] = let


class Eval_type(Protocol):
    @abc.abstractmethod
    def type(module, name, definition):
        ...


def type(name, definition):
    return lambda module: module.type(name, definition)


dispatcher[type_k] = type


class Eval_intersect(Protocol):
    @abc.abstractmethod
    def intersect(module, left, right):
        ...


def intersect(left, right):
    return lambda module: module.intersect(left, right)


dispatcher[and_k] = intersect


class Eval_alt(Protocol):
    @abc.abstractmethod
    def alt(module, left, right):
        ...


def alt(left, right):
    return lambda module: module.alt(left, right)


dispatcher[or_k] = alt


class Eval_quote(Protocol):
    @abc.abstractmethod
    def quote(module, contents):
        ...


def quote(contents):
    return lambda module: module.quote(contents)


dispatcher[quote_k] = quote


class Eval_unquote(Protocol):
    @abc.abstractmethod
    def unquote(module, contents):
        ...


def unquote(contents):
    return lambda module: module.unquote(contents)


dispatcher[unquote_k] = unquote


class Eval_lam(Protocol):
    @abc.abstractmethod
    def lam(module, arg, type, ret):
        ...


def lam(arg, type, ret):
    return lambda module: module.lam(arg, type, ret)


dispatcher[lambda_k] = lam


class Eval_match(Protocol):
    @abc.abstractmethod
    def match(module, target, cases: builtins.list):
        ...


def match(target, cases: builtins.list):
    return lambda module: module.match(target, cases)


dispatcher[match_k] = match


class Eval_annotate(Protocol):
    @abc.abstractmethod
    def annotate(module, var, type):
        ...


def annotate(var, type):
    return lambda module: module.annotate(var, type)


dispatcher[ann_k] = annotate


class Eval_binary(Protocol):
    @abc.abstractmethod
    def binary(module, head, tl: Tuple[builtins.tuple, ...]):
        ...


def binary(head, tl: Tuple[builtins.tuple, ...]):
    return lambda module: module.binary(head, tl)


dispatcher[bin_k] = binary


class Eval_case(Protocol):
    @abc.abstractmethod
    def case(module, pat, expr):
        ...


def case(pat, expr):
    return lambda module: module.case(pat, expr)


dispatcher[case_k] = case


class Eval_list(Protocol):
    @abc.abstractmethod
    def list(module, elts):
        ...


def list(elts):
    return lambda module: module.list(elts)


dispatcher[list_k] = list


class Eval_tuple(Protocol):
    @abc.abstractmethod
    def tuple(module, elts):
        ...


def tuple(elts):
    return lambda module: module.tuple(elts)


dispatcher[tuple_k] = tuple


class Eval_record(Protocol):
    @abc.abstractmethod
    def record(module, pairs, row):
        ...


def record(pairs, row):
    return lambda module: module.record(pairs, row)


dispatcher[record_k] = record


class Eval_call(Protocol):
    @abc.abstractmethod
    def call(module, f, arg):
        ...


def call(f, arg):
    return lambda module: module.call(f, arg)


dispatcher[call_k] = call


class Eval_arrow(Protocol):
    @abc.abstractmethod
    def arrow(module, arg, ret):
        ...


def arrow(arg, ret):
    return lambda module: module.arrow(arg, ret)


dispatcher[arrow_k] = arrow


class Eval_imply(Protocol):
    @abc.abstractmethod
    def imply(module, arg, ret):
        ...


def imply(arg, ret):
    return lambda module: module.imply(arg, ret)


dispatcher[imply_k] = imply


class Eval_loc(Protocol):
    @abc.abstractmethod
    def loc(module, location, contents):
        ...


def loc(location, contents):
    return lambda module: module.loc(location, contents)


dispatcher[loc_k] = loc


class Eval_forall(Protocol):
    @abc.abstractmethod
    def forall(module, fresh_vars: builtins.tuple, polytype):
        ...


def forall(fresh_vars: builtins.tuple, polytype):
    return lambda module: module.forall(fresh_vars, polytype)


dispatcher[forall_k] = forall


class Eval_exist(Protocol):
    @abc.abstractmethod
    def exist(module, bound_vars: builtins.tuple, monotype):
        ...


def exist(bound_vars: builtins.tuple, monotype):
    return lambda module: module.exist(bound_vars, monotype)


dispatcher[exist_k] = exist


class Eval_guard(Protocol):
    @abc.abstractmethod
    def guard(module, expr):
        ...


def guard(expr):
    return lambda module: module.guard(expr)


dispatcher[guard_k] = guard


class Eval_pin(Protocol):
    @abc.abstractmethod
    def pin(module, expr):
        ...


def pin(expr):
    return lambda module: module.pin(expr)


dispatcher[pin_k] = pin


class Eval_uncall(Protocol):
    @abc.abstractmethod
    def uncall(module, f, arg):
        ...


def uncall(f, arg):
    return lambda module: module.uncall(f, arg)


dispatcher[uncall_k] = uncall


class Eval_cons(Protocol):
    @abc.abstractmethod
    def cons(module, branches):
        ...


def cons(branches):
    return lambda module: module.cons(branches)


dispatcher[cons_k] = cons


class Eval_attr(Protocol):
    @abc.abstractmethod
    def attr(module, base, attr_name: builtins.str):
        ...


def attr(base, attr_name: builtins.str):
    return lambda module: module.attr(base, attr_name)


dispatcher[attr_k] = attr


class Eval_module(Protocol):
    @abc.abstractmethod
    def module(module_eval, is_rec, name, stmts):
        ...


def module(is_rec, name, stmts):
    return lambda module_eval: module_eval.module(is_rec, name, stmts)


dispatcher[module_k] = module


class Eval_mutual(Protocol):
    @abc.abstractmethod
    def mutual(module, cases, expr):
        ...


def mutual(cases, expr):
    return lambda module: module.mutual(cases, expr)


dispatcher[mutual_k] = mutual


class Eval_extern(Protocol):
    @abc.abstractmethod
    def extern(module, foreign_code):
        ...


def extern(foreign_code):
    return lambda module: module.extern(foreign_code)


dispatcher[extern_k] = extern


class Eval_ite(Protocol):
    @abc.abstractmethod
    def ite(module, cond, true_clause, else_clause):
        ...


def ite(cond, true_clause, else_clause):
    return lambda module: module.ite(cond, true_clause, else_clause)


dispatcher[ite_k] = ite


class Eval_imp(Protocol):
    @abc.abstractmethod
    def imp(module, qualname):
        ...


def imp(qualname):
    return lambda module: module.imp(qualname)


dispatcher[import_k] = imp


class Eval_set(Protocol):
    @abc.abstractmethod
    def set(module, sym, expr):
        ...


def set(sym, expr):
    return lambda module: module.set(sym, expr)


dispatcher[set_k] = set


class Eval_inst(Protocol):
    @abc.abstractmethod
    def inst(module, type, scope, expr):
        ...


def inst(type, scope, expr):
    return lambda module: module.inst(type, scope, expr)


dispatcher[inst_k] = inst


class Eval_typed(Protocol):
    @abc.abstractmethod
    def typed(module, type, expr):
        ...


def typed(type, expr):
    return lambda module: module.typed(type, expr)


dispatcher[typed_k] = typed


class Eval_func(Protocol):
    @abc.abstractmethod
    def func(module, name, filename, freevars, arg, expr):
        ...


def func(name, filename, freevars, arg, expr):
    return lambda module: module.func(name, filename, freevars, arg, expr)


dispatcher[func_k] = func


class Eval_invoke(Protocol):
    @abc.abstractmethod
    def invoke(module, f, arg):
        ...


def invoke(f, arg):
    return lambda module: module.invoke(f, arg)


dispatcher[invoke_k] = invoke


class Eval_prj(Protocol):
    @abc.abstractmethod
    def prj(module, expr, i):
        ...


def prj(expr, i):
    return lambda module: module.prj(expr, i)


dispatcher[prj_k] = prj


class Eval_label(Protocol):
    @abc.abstractmethod
    def label(module, name):
        ...


def label(name):
    return lambda module: module.label(name)


dispatcher[label_k] = label


class Eval_goto(Protocol):
    @abc.abstractmethod
    def goto(module, name):
        ...


def goto(name):
    return lambda module: module.goto(name)


dispatcher[goto_k] = goto


class Eval_goto_if(Protocol):
    @abc.abstractmethod
    def goto_if(module, name, cond):
        ...


def goto_if(name, cond):
    return lambda module: module.goto_if(name, cond)


dispatcher[goto_if_k] = goto_if


class Eval_goto_if_not(Protocol):
    @abc.abstractmethod
    def goto_if_not(module, name, cond):
        ...


def goto_if_not(name, cond):
    return lambda module: module.goto_if_not(name, cond)


dispatcher[goto_if_not_k] = goto_if_not


class Eval_indir(Protocol):
    @abc.abstractmethod
    def indir(module, expr):
        ...


def indir(expr):
    return lambda module: module.indir(expr)


dispatcher[indir_k] = indir


class Eval_addr(Protocol):
    @abc.abstractmethod
    def addr(module, name):
        ...


def addr(name):
    return lambda module: module.addr(name)


dispatcher[addr_k] = addr


class Eval_block(Protocol):
    @abc.abstractmethod
    def block(module, elts):
        ...


def block(elts):
    return lambda module: module.block(elts)


dispatcher[block_k] = block


class Eval_switch(Protocol):
    @abc.abstractmethod
    def switch(module, target, cases, default):
        ...


def switch(target, cases, default):
    return lambda module: module.switch(target, cases, default)


dispatcher[switch_k] = switch
