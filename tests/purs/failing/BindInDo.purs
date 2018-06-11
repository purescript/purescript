-- @shouldFailWith CannotUseBindWithDo
module Main where

import Prelude

foo = do
  bind <- [1,2,3]
  x <- [4, 5, 6]
  pure x
