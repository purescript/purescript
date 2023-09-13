-- @shouldFailWith MissingClassMember
module Main where

import Prim.TypeError

class Foo t where
  foo :: t -> String
  bar :: Int -> t

instance fooInt :: Fail (Text "can't use this") => Foo Int where
  foo _ = "unreachable"
  -- bar is missing; you can get away with an empty instance here but not a
  -- half-implemented one
