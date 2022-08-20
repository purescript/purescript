-- @shouldFailWith InvalidInstanceHead
module Main where

import Prelude

derive instance eqRecord :: Eq {}
