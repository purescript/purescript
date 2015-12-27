This error occurs when a pattern matching definition has **non-exhaustive** patterns.

As an example of this situation, consider the following definition:

```haskell
f :: Number -> Number
f 0 = 0
```

This is clearly not exhaustive, as it fails to cover all the cases for its argument type: if we apply `f` to a value it cannot match (for example `1`) we will get an error at runtime:

```
> f 1

Failed pattern match
```

Such functions are called *partial*, because they are not defined for all inputs: otherwise they are called *total*.

Another example, `Data.Either.Unsafe` exports a function called `fromLeft`:

```haskell
fromLeft :: forall a b. Either a b -> a
fromLeft (Left a) = a
```

The exhaustivity checker will throw the following warning:

```
Warning in module Data.Either.Unsafe:
  Warning in value declaration fromLeft:
  Warning at /home/travis/build/purescript/purescript/core-tests/bower_components/purescript-either/src/Data/Either/Unsafe.purs line 9, column 1 - line 10, column 1:
    Pattern could not be determined to cover all cases.
    The definition has the following uncovered cases:

      (Data.Either.Right _)
```

The solution is to make your functions total in some way. We can use the type `Maybe a` to return `Nothing` in case of a missing case:

```haskell
f_total :: Number -> Maybe Number
f_total 0 = Just 0
f_total _ = Nothing
```

The compiler will not complain to this new definition for `f`.

Up to now, we support exhaustivity checking for Data Constructors, Objects and Literals. If you are keen on using guards, you have to add an `otherwise` or `true` guard case to ensure exhaustivity.

An example with guards:

```haskell
data Nat = Zero | Succ Nat

isZero :: Nat -> Boolean
isZero x | x == Zero = true
         | otherwise = false
```
