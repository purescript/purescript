* Implement visible type applications and abstractions

  This feature adds support for visible type applications and
  abstractions, partially derived from the [Visible Type
  Application](https://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf)
  paper by Richard Eisenberg.

  Expressions can now be applied to types using `@`-based syntax, similar to
  GHC's `TypeApplications` extension:
  ```purs
  id :: forall @a. a -> a
  id a = a

  id' :: Int -> Int
  id' = id @Int
  ```

  Note that for a polytyped expression to be applied to a type, at least
  one of its type variable bindings must be prefixed with a `@`, denoting
  that a type variable can be bound using visible type application syntax.
  For example, the following snippet will fail to compile:
  ```purs
  idFail :: forall a. a -> a
  idFail a = a

  idFail' = idFail @Int
  ```

  ```purs
  Error found:
  in module Main
  at Main.purs:6:11 - 6:22 (line 6, column 11 - line 6, column 22)

    An expression of type:

      forall a. a -> a

    cannot be applied to the type:

      Int


  while inferring the type of idFail
  in value declaration idFail'

  See https://github.com/purescript/documentation/blob/master/errors/CannotApplyExpressionOfTypeOnType.md for more information,
  or to contribute content related to this error.
  ```

  By default, the type variables present in a `data` or `newtype` constructor
  can be eliminated using visible type applications, without needing to prefix
  them with `@`. They can also be skipped using a type wildcard:
  ```purs
  data Either a b = Left a | Right b

  left :: Either Int String
  left = Left @Int @String 0

  right :: Either Int String
  right = Right @Int @String "0"

  -- Either Int String
  leftSkip = Left @_ @String 0

  -- Either String Int
  rightSkip = Right @String @_ 0
  ```

  Like with `data`/`newtype`, type variables are available for type application
  by default in `class` declarations. Instead, `@` is used to determine whether
  a type variable can be skipped during type application:
  ```purs
  class Functor f where
    map :: forall a b. (a -> b) -> (f a -> f b)

  -- map :: forall @f a b. Functor f => (a -> b) -> (f a -> f b)

  map' :: (a -> b) -> (Array a -> Array b)
  map' = map @Array

  class MonadState s @m | m -> s where
    state :: forall a. (s -> (Tuple a s)) -> m a

  state' = state @Int @_  -- Invalid!
  ```

  Kind applications are now also exposed syntactically:
  ```purs
  foreign import data IdK :: forall a. a -> a

  type IntId :: Int -> Int
  type IntId = IdK @Int

  type NumberId :: Number -> Number
  type NumberId = IdK @Number
  ```

  The following errors have been added:
  1. `CannotApplyExpressionOfTypeOnType`
  2. `CannotSkipTypeApplication`
  3. `CannotApplyTypeOnType`
  4. `OnlyPartiallyDetermined`, replacing `UnusableDeclaration`
