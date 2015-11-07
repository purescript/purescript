-- @shouldFailWith TypesDoNotUnify
module X where

class Foo where
  foo :: String

instance f :: Foo where
  foo = "a"
    where
    bar :: String
    bar = 1
