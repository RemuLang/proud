from proud.core_lang import composable_evaluator as ce
from proud.core_lang.modular_compiler import CompilerCtx
from proud.core_lang.sexpr import is_ast
from proud.core_lang.scope import Sym, Scope
from proud.core_lang import types
from proud.backend_interface import BackEnd, ModuleFinder
from hybridts import type_encoding as te
import json
import typing
import os

dispatcher = ce.dispatcher


class Numbering(dict):
    def __missing__(self, key):
        v = self[key] = len(self)
        return v


class PyBackEnd(BackEnd):
    top: typing.Optional[CompilerCtx]

    def mk_top_scope(self) -> Scope:
        if not self.top:
            raise ValueError
        return self.top.scope.sub_scope(hold_bound=True)

    def remember_module(self, path, mod_sym: Sym) -> None:
        self.modules[path].exist = mod_sym

    def __init__(self):
        self.modules: typing.Dict[str, ModuleFinder] = {}
        self.top = None

    def search_module(self, qualname: typing.List[str]) -> ModuleFinder:
        path = '.'.join(qualname)
        mod = self.modules.get(path)
        if mod:
            return mod
        paths = os.environ.get("PROUD_PATH", '').split(';') + ['.']
        *init, end = qualname
        end += '.prd'

        def mk_get_code(filename):
            def apply():
                with open(filename) as f:
                    return f.read()

            return apply

        for search_path in paths:
            filename = os.path.join(os.path.abspath(search_path), *init, end)
            if os.path.exists(filename):

                return ModuleFinder(exist=None,
                                    get_code=mk_get_code(filename),
                                    filename=filename)
        raise IOError("Unknown module {}".format(path))

    def init_compiler_ctx(self, filename, path) -> CompilerCtx:
        comp_ctx: CompilerCtx = CompilerCtx.top("builtin", "builtin", self)
        sym = comp_ctx.scope.enter("int")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.int_t)
        sym = comp_ctx.scope.enter("string")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.string_t)
        sym = comp_ctx.scope.enter("bool")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.bool_t)
        sym = comp_ctx.scope.enter("float")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.float_t)
        sym = comp_ctx.scope.enter("unit")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.unit_t)
        sym = comp_ctx.scope.enter("value")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.type_type)
        sym = comp_ctx.scope.enter("list")
        comp_ctx.tenv[sym] = te.App(types.type_type, types.list_t)
        self.top = comp_ctx
        return CompilerCtx(self.mk_top_scope(), comp_ctx.tc_state,
                           comp_ctx.tenv, filename, path, self)

    def codegen(self, compiler_ctx: CompilerCtx, sexpr) -> str:
        generator = CodeGen(compiler_ctx.filename, backend=self)
        generator.eval(sexpr)
        return generator.feed_code()


class CodeGen(ce.Eval_set, ce.Eval_func, ce.Eval_invoke, ce.Eval_loc,
              ce.Eval_prj, ce.Eval_label, ce.Eval_goto, ce.Eval_goto_if,
              ce.Eval_goto_if_not, ce.Eval_indir, ce.Eval_addr, ce.Eval_block,
              ce.Eval_tuple, ce.Eval_list, ce.Eval_switch, ce.Eval_extern):
    def extern(module, foreign_code):
        module("const #{}#".format(
            json.decoder.py_scanstring(foreign_code, 1)[0]))

    def __init__(self, filename: str, backend: PyBackEnd):
        self.code = [
            'runtime operator', 'filename {}'.format(json.dumps(filename))
        ]
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
        module("args [{}] {{".format(module.s2n(arg)))
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

    def feed_code(self):
        self.code.append('const #None#')
        self.code.append('return')
        return '\n'.join(self.code)
