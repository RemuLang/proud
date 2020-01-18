from proud import excs
from proud.core_lang import sexpr
from proud.core_lang import lowered_ir as ir
from proud.core_lang import composable_evaluator as ce
from proud.core_lang import types
from proud.core_lang.scope import Sym
from proud.core_lang.modular_compiler import *
from proud.core_lang.polymorphisms import generalise_type, resolve_instance_
from proud.parser.parser_wrap import parse
import proud.unification.type_encode as te
import typing


def _sort_sym(expr: ir.Expr):
    assert isinstance(expr.expr, Sym)
    return expr.expr.name


def make(cgc: CompilerGlobalContext):
    tc_state = cgc.tc_state
    backend = cgc.backend
    tenv = cgc.tenv
    infer = tc_state.infer
    unify = tc_state.unify
    inst = tc_state.inst
    Typing = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)
    Express = typing.cast(typing.Callable[[CompilerLocalContext], Interpreter], None)

    def link_typing(evaluators):
        nonlocal Typing
        Typing = evaluators['Typing']

    def link_express(evaluators):
        nonlocal Express
        Express = evaluators['Express']

    def type_of_value(sym: Sym):
        return tenv[sym]

    class Modular(Interpreter, ce.Eval_module, ce.Eval_loc, ce.Eval_define, ce.Eval_type, ce.Eval_imp, ce.Eval_tellme):

        def eval(self, x) -> ir.Expr:
            try:
                assert sexpr.is_ast(x)
                hd, *args = x
                return ce.dispatcher[hd](*args)(self)
            except excs.StaticCheckingFailed:
                raise
            except Exception as e:
                raise excs.StaticCheckingFailed(e, self.clc)

        def imp(module, qualname: typing.List[str], *, backend=backend):
            clc = module.clc
            name = qualname[-1]
            sym = clc.scope.enter(name)
            module_finder = backend.search_module(qualname)
            if module_finder.exist:
                sym_mod = module_finder.exist
                type_mod = type_of_value(sym_mod)
                exp_mod = ir.Expr(expr=sym_mod, type=type_mod)
                return ignore(ir.Set(module_finder.exist, exp_mod))

            filename, get_code = module_finder.filename, module_finder.get_code
            path = '.'.join(qualname)
            new_modular = Modular(CompilerLocalContext(scope=backend.mk_top_scope(), filename=filename, path=path))

            ast_mod = parse(get_code(), filename)
            exp_mod = new_modular.eval(ast_mod)
            tenv[sym] = exp_mod.type
            backend.remember_module(path, sym)
            return ignore(ir.Set(sym, exp_mod))

        def type(module, typename, typedef):
            clc = module.clc
            path = clc.path
            loc, typename = sexpr.unloc(typename)
            qual_typename = '{}.{}'.format(path, typename)
            sym_typename = clc.scope.enter(typename)
            if typedef:
                # type alias
                nom_type = tc_state.infer(Typing(clc).eval(typedef))
            else:
                nom_type = types.Nom(qual_typename, loc=loc, filename=clc.filename)
            tenv[sym_typename] = te.App(types.type_type, nom_type)

            return wrap_loc(loc, ignore(ir.Set(sym_typename, ir.Expr(type=nom_type, expr=ir.Const(nom_type)))))

        def loc(module, location: Location, contents):
            module.clc.location = location
            exp = module.eval(contents)
            return wrap_loc(location, exp)

        def tellme(self, name, _):
            sym = self.clc.scope.require(name)
            backend.types_to_show.append((self.clc.location, self.clc.filename, sym))
            return ignore(ir.Const(()))

        def module(module_eval, is_rec, name, stmts, loc=None):
            loc, name = sexpr.unloc(name)
            clc = module_eval.clc
            scope = clc.scope
            filename = clc.filename
            path = clc.path = '.'.join((*filter(None, clc.path.split('.')), name))

            # only for recursive modules.
            sym_mod: typing.Optional[Sym] = None

            # generated ir outside module;
            # only for recursive modules.
            outer_stmts: typing.List[ir.Expr] = []
            # generated ir for module itself
            inner_stmts: typing.List[ir.Expr] = []

            # type of module. will be a non-extensible record.
            type_mod = None

            # recursive module.
            if is_rec:
                sym_mod = scope.enter(name)
                new_var = tc_state.new_var(Var=types.Var, loc=loc, filename=filename, name=path)
                type_mod = tenv[sym_mod] = new_var
                outer_stmts.append(ignore(ir.Set(sym_mod, ignore(ir.Const(())))))

            # for preprocessing
            # 1. recursive functions and
            # 2. type definitions.
            filter_stmts = []
            for stmt in stmts:
                _, stmt_unloc = sexpr.unloc(stmt)

                # type definition
                if stmt_unloc[0] is sexpr.type_k:
                    inner_stmts.append(module_eval.eval(stmt))
                    continue

                filter_stmts.append(stmt)
                if stmt_unloc[0] is sexpr.def_k:
                    if stmt_unloc[1]:
                        # Recursive definition. It's guaranteed to
                        # be a function in syntax level.
                        loc, n = sexpr.unloc(stmt_unloc[2])
                        sym = scope.enter(n)
                        tenv[sym] = tc_state.new_var(Var=types.Var, loc=loc, filename=filename, name=n)

            for stmt in filter_stmts:
                inner_stmts.append(module_eval.eval(stmt))

            syms = [sym for sym in scope.get_newest_bounds() if sym is not sym_mod]

            row_mod = te.row_from_list([(s.name, tenv[s]) for s in syms], te.empty_row)
            if type_mod:
                # recursive module
                tc_state.unify(type_mod, te.Record(row_mod))
            else:
                type_mod = te.Record(row_mod)

            elts_mod = [ir.Expr(type=tenv[sym], expr=sym) for sym in syms]
            elts_mod.sort(key=_sort_sym)

            val_mod = ir.Expr(type=type_mod, expr=ir.Tuple([anyway(ir.Tuple(elts_mod)), anyway(ir.Tuple([]))]))
            inner_stmts.append(val_mod)

            exp_mod = ir.Expr(type=type_mod, expr=ir.Block(inner_stmts))

            if not is_rec:
                return exp_mod

            outer_stmts.append(ignore(ir.Set(sym_mod, exp_mod)))
            outer_stmts.append(ir.Expr(type=type_mod, expr=sym_mod))
            return ir.Expr(type=type_mod, expr=ir.Block(outer_stmts))

        def define(self, is_rec, name, annotated, bound):
            module = Express(self.clc)
            loc, name = sexpr.unloc(name)
            clc = self.clc
            outer_scope = clc.scope

            if is_rec:
                sym_def = outer_scope.require(name)
            else:
                sym_def = outer_scope.shadow(name)  # may shadow

            with clc.resume_scope():
                inner_scope = outer_scope.sub_scope()

                clc.scope = inner_scope
                if annotated:
                    t1 = infer(Typing(clc).eval(annotated))
                    if sym_def in tenv:
                        unify(inst(tenv[sym_def])[1], inst(t1)[1])
                    else:
                        tenv[sym_def] = t1
                else:
                    if sym_def in tenv:
                        t1 = infer(tenv[sym_def])
                    else:
                        new_var = tc_state.new_var(Var=types.Var, loc=loc, filename=clc.filename, name=sym_def.name)
                        tenv[sym_def] = t1 = new_var

                level = tc_state.push_level()

                t1_inst = te.fresh(tc_state.new_var, t1)
                t1_inst = inst(t1_inst, rigid=True)[1]

                bound: ir.Expr = module.eval(bound)
                t2_inst = bound.type = inst(infer(bound.type))[1]
                resolve_instance_(outer_scope, bound)
                unify(t1_inst, t2_inst)
                bound.type = generalise_type(tc_state, t2_inst, loc=loc, filename=clc.filename, level=level)

            unify(bound.type, t1)

            exp_def = ir.Set(sym_def, bound)
            return ignore(exp_def)

    return Modular, [link_express, link_typing]
