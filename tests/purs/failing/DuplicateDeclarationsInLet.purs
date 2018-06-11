-- @shouldFailWith OverlappingNamesInLet
module Main where

import Prelude

foo = a
  where
  a :: Number
  a = 1

  a :: Number
  a = 2
