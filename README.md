# Proud Programming Language

Hope we can see a to-Python compiler in one day :)
 
## Feature

- Row polymorphisms(Extensible Records)! 
- First class polymorphisms!
- Type classes/traits!
- Modules as records!
- Type holes(use `exist a. a` to create a scoped type variable for completing types for you)
- Implicit inference(only first class polymorphisms need manually annotations)
- Customizing operators' precedence and associativity(it's safe, because if you don't have to open a module globally), by [Remu-Operator](https://github.com/RemuLang/remu-operator).

## Overview

Currently, the strong type system is finished.

A core language is finished, with a code generation interface and implementation in Python.

`example_code/hello_world.prd`:

```ocaml
module HW

# extensible records(compiling to tuples!!)
let print: forall a. a -> unit = extern "print"
let add : forall a. a ->  a -> a = extern "lambda a: lambda b: add(a, b)"
let hello_world: forall rho. {name: string | rho} -> unit = fun r ->
    print (add "hello " r.name)

let _ = hello_world {name = "thautwarm", age = 18, sex = 0.5 }

# quotation and splicing
let var = "b"
let code = `(print ($add x $var))
let str_print : string -> unit = print
let runcode = code {x = "n", print = str_print}
``` 

To run this file:
```shell script
sh> python checkprd.py example_code/hello_world.prd
sh> sij cc example_code/hello_world.prd.sij example_code/hello_world.prd.pyc
sh> python example_code/hello_world.prd.pyc
hello thautwarm
nb
```

## Run tests

Check examples at `example_code/`, use `python checkprd.py <path>` to compile proud source to sijuiacion IR,
use `sij run <sij ir script>` to run the IR or `sij cc <sij ir script>` to compile it to Python `.pyc` file.
