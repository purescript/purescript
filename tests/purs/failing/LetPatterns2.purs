-- @shouldFailWith UnknownName
module Main where

import Prelude

data X a = X a

-- wrong dependency order
x =
  let
    b = a
    X a = X 10
  in
   b
