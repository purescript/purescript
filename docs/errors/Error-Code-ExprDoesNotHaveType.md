This error occurs when the type checker thinks an expression needs to have a certain type to type-check, but it has a different type.

For example:

```purescript
x = true <> "hello"
```

The type of `(<>)` is `forall a. (Semigroup a) => a -> a -> a`, which means that in order to type-check, both of its arguments must have the same type. However, the type of `true` is `Boolean`, and the type of `"hello"` is `String`. These are different types, so the type checker rejects this program.