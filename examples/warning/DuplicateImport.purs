-- @shouldWarnWith DuplicateImport
module Main where

import Prelude (Unit, unit, pure)
import Prelude (Unit, unit, pure)

import Control.Monad.Eff (Eff)

main :: Eff () Unit
main = pure unit
