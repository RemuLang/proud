# Steps

## Step0: Code Gen Phase

```shell script
SExpr ::= (let_k Sym SExpr)
```
  
## Step1: A minimal core language

- pattern matching: only `guard` is implemented.
- `let`: LHS must be a symbol or a symbol with type annotation
- type definition: Only `type a` is allowed, no `ADT` syntax.

Other language features and constructs are implemented.
  

