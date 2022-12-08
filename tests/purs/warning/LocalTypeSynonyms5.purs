-- @shouldWarnWith UnusedName
-- @shouldWarnWith ShadowedTypeVar
module Main where

test :: forall a. a -> a
test x = x
  where
  type Foo (f :: forall a. a -> a) = f Boolean
