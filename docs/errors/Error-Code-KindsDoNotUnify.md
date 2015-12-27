# Cannot unify kind * with kind # *
This can happen when a function is declared without kind `*`, like a function that attempts to return a row:

```purs
onClick :: forall a. String -> (click :: String | a)
onClick s = {click: s}
```

The error message reads along the lines of:
```purs
  Prim.String -> (click :: Prim.String | a)

  Cannot unify kind
    *
  with kind
    # *
```

**Prime suspect:** You are missing an `->` in your type signature.


# Cannot unify kind * with kind * -> *
This error occurs when you try adding a type class instance with a superfluous type parameter, e.g. (from the Purescript by Example book):
```purs
data NonEmpty a = NonEmpty a [a]

instance functorNonEmpty :: Functor (NonEmpty a) where
  (<$>) f (NonEmpty x xs) = NonEmpty (f x) []
```
The correct version is (just use `Functor NonEmpty` instead of `Functor (NonEmpty a)`):
```purs
instance functorNonEmpty :: Functor NonEmpty where
  (<$>) f (NonEmpty x xs) = NonEmpty (f x) []
```
