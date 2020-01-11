from proud import composable_evaluator as ce
from proud.scope import Sym
import json

dispatcher = ce.dispatcher


class Codegen(ce.Eval_set, ce.Eval_func, ce.Eval_invoke, ce.Eval_loc,
              ce.Eval_prj, ce.Eval_label, ce.Eval_goto, ce.Eval_goto_if,
              ce.Eval_goto_if_not, ce.Eval_indir, ce.Eval_addr, ce.Eval_block,
              ce.Eval_tuple, ce.Eval_list, ce.Eval_switch):
    def __init__(self):
        self.code = []
        self.layout = 0

    def __call__(self, other: str):
        self.code.append('  ' * self.layout + other)

    def eval(self, node):
        hd, *args = node
        return dispatcher[hd](*args)(self)

    def loc(module, location, contents):
        if location:
            line = location[0]
            module('line {}'.format(line))
        module.eval(contents)

    def set(module, sym: str, expr):
        assert isinstance(sym, Sym)
        module.eval(expr)
        module("store {}_{}".format(sym.name, sym.uid))

    def invoke(module, f, arg):
        module.eval(f)
        module.eval(arg)
        module("call 1")

    def prj(module, expr, i):
        module.eval(expr)
        module.eval(i)
        module("prj")

    def label(module, name):
        module("label {}".format(name))

    def goto(module, name):
        module("goto {}".format(name))

    def goto_if(module, name, cond):
        module.eval(cond)
        module("goto-if {}".format(name))

    def goto_if_not(module, name, cond):
        module.eval(cond)
        module("goto-if-not {}".format(name))

    def indir(module, expr):
        module.eval(expr)
        module("indir")

    def addr(module, name):
        module("blockaddr {}".format(name))

    def block(module, elts):
        for each in elts:
            module.eval(each)
            module("pop")
        module.code.pop()

    def tuple(module, elts):
        for each in elts:
            module.eval(each)
        module("tuple {}".format(len(elts)))

    def list(module, elts):
        for each in elts:
            module.eval(each)
        module("list {}".format(len(elts)))

    def func(module, name, filename, freevars, arg, expr):
        module("defun")
        module.layout += 2

        module("filename {}".format(json.dumps(filename)))
        module("free [{}]".format(' '.join(freevars)))
        module("args [{}] {".format(arg))
        module.eval(expr)
        module("label pround.return")
        module("return")
        module.layout -= 2
        module("}")

    def switch(module, target, cases, default):
        module.eval(target)
        module("switch")
        for i, n in cases:
            module("| {} => {}".format(i, n))
        if default:
            module("| _ => {}".format(default))
