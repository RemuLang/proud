# Notes

## Step0: Code Gen Phase

  
## Step1: A minimal core language

- pattern matching: only `guard` is implemented.
- `let`: LHS must be a symbol or a symbol with type annotation
- type definition: Only `type a` is allowed, no `ADT` syntax.

Other language features and constructs are implemented.

P.S: It seems also a bit time-consuming.

We don't implement `open` statements now.

### 1/15/2020

Incidentally, we gain `decltype` in prond, it can be used like parametric type aliases,
and still incidentally, it provides us kind checker.

Now it comes to the implementation of type classes!

For instance, `{f : forall a. a -> int}` and `{f : forall a -> a}`, compute the principal type, with existing framework?

Say, we have `| {f : int -> int} | => f 2`  
  
for type `{f : int -> int}`, we know it has principal types:
    - `A = { f:  forall a. a -> int }`
    - `B = { f:  forall a. int -> a}`
    - `C = { f : forall a. a -> a}`
    - `D = { f : forall a b. a -> b }`
    - `E = { f : int -> forall a. a }`
    - `F = { f:  (forall a . a) -> int }`
    - `G = { f : (forall a. a) -> (forall a. a)`
    - `H = { f : forall a. a }` (bottom)
    where `A >= C, B >= C, C >= D, E >= G, F >= G`, we should compute principal recursively.
 
Given 2 types `a, b`, to check if `a <= b`, we generate a tree for all lesser types of `b`, and see if `a` is in the `tree`.

      