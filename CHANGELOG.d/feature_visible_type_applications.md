* Implement visible type applications and abstractions

This feature adds support for visible type applications and
abstractions, partially derived from the [Visible Type
Application](https://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf)
paper by Richard Eisenberg.

Expressions can now be applied to types using `@`-based syntax, similar to
GHC's `TypeApplications` extension:
```hs
id :: forall @a. a -> a
id a = a

id' :: Int -> Int
id' = id @Int
```

Note that for a polytyped expression to be applied to a type, at least
one of its type variable bindings must be prefixed with a `@`, denoting
that a type variable can be bound using visible type application syntax.

```hs
Error found:
in module Main
at Main.purs:6:7 - 6:14 (line 6, column 7 - line 6, column 14)

  An expression of type:

    forall a. a -> a

  cannot be applied to the type:

    Int


while inferring the type of id
in value declaration id'

See https://github.com/purescript/documentation/blob/master/errors/CannotApplyExpressionOfTypeOnType.md for more information,
or to contribute content related to this error.
```

`@`-syntax can also be used in class and data heads, like so:
```hs
data Either @a @b = Left a | Right b

left :: Either Int String
left = Left @Int @String 0

right :: Either Int String
right = Right @Int @String "0"

class Functor @f where
  map :: forall a b. (a -> b) -> (f a -> f b)

map' :: (a -> b) -> (Array a -> Array b)
map' = map @Array
```
