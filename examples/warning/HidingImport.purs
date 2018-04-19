-- @shouldWarnWith HidingImport
-- @shouldWarnWith HidingImport
module Main where

import Prelude hiding (one)
import Effect hiding (runPure)

main :: Effect Unit
main = pure unit
