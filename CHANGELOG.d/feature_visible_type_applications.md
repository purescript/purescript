* Implement visible type applications

  The compiler now supports visible type applications, allowing the user to instantiate one or more "visible" type variables to a specific type.

  A "visible" type variable is a type variable in a `forall` binder that appears prefixed with an `@`, like the following example:

  ```purescript
  id :: forall @a. a -> a  -- or with kinds: `forall (@a :: Type). a -> a`
  id a = a
  ```

  We can then use type application syntax to instantiate this binding to a specific type:

  ```purescript
  idInt :: Int -> Int
  idInt = id @Int

  example :: Int
  example = id @Int 0
  ```

  Type variables appearing in `class` or `data` are automatically visible, meaning that they do not require annotations:

  ```purescript
  data Maybe a = Just a | Nothing

  nothingInt :: Maybe Int
  nothingInt = Nothing @Int

  class Identity a where
    identity :: a -> a

  instance Identity Int where
    identity a = a
  
  identityInt = identity @Int

  -- This throws a `NoInstanceFound` error.
  identityNumber = identity @Number
  ```

  Lastly, visible type variables can also be skipped with a wildcard (i.e. `_`)

  ```purescript
  data Either a b = Left a | Right b

  example = Left @_ @Number 0
  ```

  Note that performing a type application with a type that has no visible type variables throws an error:

  ```purescript
  module Main where

  id :: forall a. a -> a
  id a = a

  idInt = id @Int

  {-
  Error found:
  in module Main
  at Main.purs:6:9 - 6:16 (line 6, column 9 - line 6, column 16)

    An expression of polymorphic type
    with the invisible type variable a:
                    
      forall a. a -> a
                    
    cannot be applied to:
       
      Int
       

  while inferring the type of id
  in value declaration idInt

  See https://github.com/purescript/documentation/blob/master/errors/CannotApplyExpressionOfTypeOnType.md for more information,
  or to contribute content related to this error.
  -}
  ```

  Similarly, monomorphic types also cannot be used for type applications:

  ```purescript
  module Main where

  idInt :: Int -> Int
  idInt a = a

  example = idInt @Int

  {-
  Error found:
  in module Main
  at Main.purs:6:11 - 6:21 (line 6, column 11 - line 6, column 21)

    An expression of monomorphic type:
              
      Int -> Int
              
    cannot be applied to:
       
      Int
       

  while inferring the type of idInt
  in value declaration example

  See https://github.com/purescript/documentation/blob/master/errors/CannotApplyExpressionOfTypeOnType.md for more information,
  or to contribute content related to this error.
  -}
  ```
