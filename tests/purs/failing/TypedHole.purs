-- @shouldFailWith HoleInferredType
module Main where

import Prelude
import Effect (Effect)

main :: Effect Unit
main = ?ummm
