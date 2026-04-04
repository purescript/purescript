module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert)

newtype X = X String
  derive newtype (Eq, Show)

newtype Meters = Meters Number
  derive newtype (Eq, Ord, Show)

main = do
  assert $ X "hello" == X "hello"
  assert $ show (X "hi") == "\"hi\""
  assert $ Meters 1.0 < Meters 2.0
  assert $ show (Meters 3.0) == "3.0"
  log "Done"
