-- @shouldWarnWith ShadowedTypeVar
module Main where

test :: forall a. a -> a
test x = if truth then x else test x
  where
  type Foo a = Boolean
  truth :: Foo Int
  truth = true
