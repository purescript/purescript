"Better go catch it then." - Unhelpful developer.

Skolem constants are types which only unify with themselves, and a limited range of unification variables. They are used to check polymorphic types, usually when using Rank N Types.

We say that a skolem _escapes its scope_ when an attempt is made to unify it with a unification variable which was generated outside the scope in which the skolem was generated.

Consider for example, this function:

```purescript
test = do
  r <- runST (newSTRef 0)
  return 0
```

Here the type of `runST` causes us to check `newSTRef 0` against a polymorphic type, unifying the type of `r` with a skolem constant. However, the type of `r` is fresh, generated outside the scope of `newSTRef 0`, so we see an `EscapedSkolem` error. This is good, because we don't want the reference to leak outside of `runST`.

### `$` and `runST`

One common pitfall is to use the `runST` or `runPure` functions with the `$` operator. This will often lead to the `EscapedSkolem` error due to instantiation of type variables. Avoid this by using parentheses instead of `$`.