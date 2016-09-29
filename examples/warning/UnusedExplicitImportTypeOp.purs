-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Control.Monad.Eff (Eff)
import Lib (type (~>), natId)

main :: Eff () Unit
main = natId (pure unit)
