# Proud Programming Language

Hope we can see a to-Python compiler in one day :)
 
## Feature

- Row polymorphisms(Extensible Records)! 
- First class polymorphisms!
- Type classes/traits!
- Modules as records!
- Type holes(use `exist a. a` to create a scoped type variable for completing types for you)
- Implicit inference(only first class polymorphisms need manually annotations)


## Overview

Currently, the strong type system is finished.

```python 
from proud.parser_wrap import parse
from proud.basic_impl import Modular, CompilerCtx

mod = parse("""
module A
export inst = 2
type my_type
let f1 = | x | -> x
export auto : forall a. a -> a = | x | -> x
let _ = f1 inst
let choose: forall a. a -> a -> a = | x | -> | y | -> x
let res = choose auto
""")
comp_ctx = CompilerCtx.top('a.prd', 'a')
modular = Modular(comp_ctx)
xs = modular.eval(mod)
tc = comp_ctx.tc_state

for k, v in comp_ctx.tenv.items():
    print(k.name, ':', tc.infer(v))
```

gets you

```shell script
a.A.my_type : <value> <a.A.my_type from a.prd, line 4, col 1>
inst : <bigint>
x : <bigint>
f1 : <bigint> -> <bigint>
x : a
a : <value> a
auto : forall a. a -> a
_ : <bigint>
x : a
y : a
a : <value> a
choose : forall a. a -> a -> a
res : forall a. (a -> a) -> a -> a
A : {auto: forall a. a -> a, a.A.my_type: <value> <a.A.my_type from a.prd, line 4, col 1>, inst: <bigint>}
```

The LHS names are just the name of a type variable's string representation, so they're not unique.
Say, we can see there're 3 `a`s in above output, they're bounded type variables during the inference.

Moreover, those names actually have unique representations during the compilation. 