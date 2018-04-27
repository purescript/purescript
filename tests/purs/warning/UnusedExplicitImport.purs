-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure, bind)
import Effect (Effect)

main :: Effect Unit
main = pure unit
