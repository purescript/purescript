-- @shouldFailWith IncorrectConstructorArity
module Main where

import Prelude

data Person = Person String Int

data TwoPeople = Two Person Person

getName p = case p of
  (Two (Person n) (Person n2 a2)) -> n
  _ -> "Unknown"


name = getName (Two (Person "Jimmy" 20) (Person "" 1))
