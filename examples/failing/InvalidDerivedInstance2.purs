-- @shouldFailWith ExpectedTypeConstructor
module Main where

import Prelude

derive instance eqRecord :: Eq {}
