-- @shouldFailWith ClassInstanceArityMismatch
module Main where

import Prelude

data X = X

derive instance eqX :: Eq X X
