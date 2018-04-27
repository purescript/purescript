-- @shouldWarnWith UnusedImport
-- @shouldWarnWith UnusedImport
-- @shouldWarnWith UnusedImport
module Main where

import Data.Unit (Unit, unit)

-- All of the below are unused
import Effect
import Effect.Console as Console
import Test.Assert ()

main :: Unit
main = unit
