-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure, bind)
import Control.Monad.Eff (Eff)

main :: Eff () Unit
main = pure unit
