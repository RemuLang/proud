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

## Run tests

Check examples at `example_code/`.

```shell script
> python checkprd.py example_code\row_field.prd
filename:  example_code\row_field.prd
code:
module MyMod

let my : {a : bigint, p : bigint | exist tho. tho} = coerce 0
let another_field : unit = my.another


checked:
bigint : <value> <bigint>
string : <value> <string>
bool : <value> <bool>
float : <value> <float>
unit : <value> <unit>
my : {p: <bigint>, a: <bigint>, another: <unit>|<flexible var|0>}
tho : <value> {another: <unit>|<flexible var|0>}
another_field : <unit>
MyMod : {}
```