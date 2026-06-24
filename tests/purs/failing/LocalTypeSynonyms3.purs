-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

foo :: String
foo = f identity
  where
  type F x y = x -> y
  type G x = F x
  f :: G String String -> String
  f k = k "Done"
