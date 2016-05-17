-- @shouldWarnWith DuplicateSelectiveImport
module Main where

import Prelude (Unit, unit)
import Prelude (pure)

import Control.Monad.Eff (Eff)

main :: Eff () Unit
main = pure unit
