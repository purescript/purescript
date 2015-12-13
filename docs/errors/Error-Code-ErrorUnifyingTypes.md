This error occurs when the type checker is unable to deduce that two types are 'the same'.

For example, take this expression:

```purescript
[] == [unit]
```

`[]` has type `forall a. [a]`, but `[unit]` has type `[Unit]`. These types are not the same. But the type checker is able to determine that the type `Unit` may be chosen for the type variable `a`, so the types do become the same. We say that the type checker successfully unifies `forall a. [a]` with `[Unit]` in this case.

Another example:

```purescript
f :: Number -> Number
f x = x + 1
g :: Boolean -> Boolean
g x = x || true

h = g <<< f
```

The type of `(<<<)` (that is, function composition) is `forall a b c. (b -> c) -> (a -> b) -> (a -> c)`. For the right hand side of `h` to type-check, we need to find types `a`, `b`, and `c` such that the types match up. That is, we need to find a choice of `a`, `b`, and `c` such that:

* `b = Boolean` (from the argument type of `g`)
* `c = Boolean` (from the return type of `g`)
* `a = Number` (from the argument type of `f`)
* `b = Number` (from the return type of `f`).

`b` can not be `Boolean` and `Number` at the same time, so this system of equations is not satisfiable, and the type checker rejects the program.