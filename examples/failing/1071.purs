-- @shouldFailWith KindsDoNotUnify
module Main where

class Foo a b where
  foo :: a -> b

bar :: forall a. (Foo a) => a -> a
bar a = a
