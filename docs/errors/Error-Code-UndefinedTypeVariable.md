This error occurs when a type variable appears in a type signature without having first been defined.

For example, in this type signature, the type variable `a` is not defined:

```purescript
id :: a -> a
id x = x
```

A possible fix is to define the variable with a `forall` quantifier:

```purescript
id :: forall a. a -> a
id x = x
```

See also [[Differences from Haskell]].