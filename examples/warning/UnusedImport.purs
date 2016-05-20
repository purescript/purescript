-- @shouldWarnWith UnusedImport
-- @shouldWarnWith UnusedImport
-- @shouldWarnWith UnusedImport
module Main where

import Data.Unit (Unit, unit)

-- All of the below are unused
import Control.Monad.Eff
import Control.Monad.Eff.Console as Console
import Test.Assert ()

main :: Unit
main = unit
