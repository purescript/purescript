module Main where

import Prelude
import Test.Assert

mkValue :: Number -> Number
mkValue id = id

main = do
  let value = mkValue 1.0
  assert $ value == 1.0
  Control.Monad.Eff.Console.log "Done"
