module Main where

import Prelude ((==), bind)
import Test.Assert (assert)
import Control.Monad.Eff.Console (log)

singleCharacter = "\0\b\t\n\v\f\r\"\\" == "\x0\x8\x9\xA\xB\xC\xD\x22\x5C"
hex = "\x1D306\x2603\x3C6\xE0\x0" == "𝌆☃φà\0"
decimal  = "\119558\9731\966\224\0" == "𝌆☃φà\0"
surrogatePair = "\xD834\xDF06" == "\x1D306"

main = do
  assert singleCharacter
  assert hex
  assert decimal
  assert surrogatePair
  log "Done"
