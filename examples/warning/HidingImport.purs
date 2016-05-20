-- @shouldWarnWith HidingImport
-- @shouldWarnWith HidingImport
module Main where

import Prelude hiding (one)
import Control.Monad.Eff hiding (runPure)

main :: Eff () Unit
main = pure unit
