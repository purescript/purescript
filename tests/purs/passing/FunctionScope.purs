module Main where

import Prelude
import Test.Assert
import Effect.Console (log)

mkValue :: Number -> Number
mkValue id = id

main = do
  let value = mkValue 1.0
  assert $ value == 1.0
  log "Done"
