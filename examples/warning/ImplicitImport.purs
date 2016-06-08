-- @shouldWarnWith ImplicitImport
-- @shouldWarnWith ImplicitImport
module Main where

import Prelude
import Control.Monad.Eff

main :: Eff () Unit
main = pure unit
