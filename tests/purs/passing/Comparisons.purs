module Main where

import Prelude
import Effect
import Effect.Console
import Test.Assert

main = do
  assert (1.0 < 2.0)
  assert (2.0 == 2.0)
  assert (3.0 > 1.0)
  assert ("a" < "b")
  assert ("a" == "a")
  assert ("z" > "a")
  log "Done"
