-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Lib (type (~>), natId)

main :: Effect Unit
main = natId (pure unit)
