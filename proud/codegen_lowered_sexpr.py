from proud import composable_evaluator as ce
from proud.sexpr import ASTTag, is_ast
from proud.scope import Sym
import json

dispatcher = ce.dispatcher


class Numbering(dict):
    def __missing__(self, key):
        v = self[key] = len(self)
        return v


class Codegen(ce.Eval_set, ce.Eval_func, ce.Eval_invoke, ce.Eval_loc,
              ce.Eval_prj, ce.Eval_label, ce.Eval_goto, ce.Eval_goto_if,
              ce.Eval_goto_if_not, ce.Eval_indir, ce.Eval_addr, ce.Eval_block,
              ce.Eval_tuple, ce.Eval_list, ce.Eval_switch, ce.Eval_typed,
              ce.Eval_attr):
    def __init__(self):
        self.code = []
        self.layout = 0
        self.number = Numbering()

    def s2n(self, s: Sym) -> str:
        return '{}_{}'.format(s.name, id(self.number[s.uid]))

    def __call__(self, other: str):
        self.code.append('  ' * self.layout + other)

    def eval(self, node):
        if isinstance(node, Sym):
            instr = "deref" if node.is_cell.contents else "load"
            self("{} {}".format(instr, self.s2n(node)))
            return
        if is_ast(node):
            hd, *args = node
            return dispatcher[hd](*args)(self)

        self("const #{}#".format(repr(node)))

    def loc(module, location, contents):
        if location:
            line = location[0]
            module('line {}'.format(line))
        module.eval(contents)

    def set(module, sym: Sym, expr):
        assert isinstance(sym, Sym)
        module.eval(expr)
        instr = "deref!" if sym.is_cell.contents else "store"
        module("{} {}".format(instr, module.s2n(sym)))

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
        assert not freevars or isinstance(freevars[0], Sym)
        assert isinstance(arg, Sym)

        module("defun")
        module.layout += 2

        module("filename {}".format(json.dumps(filename)))
        module("free [{}]".format(' '.join(map(module.s2n, freevars))))
        module("args [{}] {".format(module.s2n(arg)))
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

    def typed(module, type, expr):
        """
        specific for row records
        """
        raise NotImplementedError

    def attr(module, base, attr_name: str):
        raise NotImplementedError
