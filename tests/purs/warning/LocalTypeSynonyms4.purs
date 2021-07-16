-- @shouldWarnWith ShadowedTypeVar
module Main where

test :: forall a. a -> a
test x = id x
  where
  type Foo = forall a. a -> a
  id :: Foo
  id y = y
