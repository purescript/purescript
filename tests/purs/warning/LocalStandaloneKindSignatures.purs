-- @shouldWarnWith UnusedName
-- @shouldWarnWith ShadowedTypeVar
module Main where

test :: forall a. a -> a
test x = x
  where
  type Foo :: forall a. a -> Type
  type Foo b = Boolean
