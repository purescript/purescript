module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assert)

data A = A

instance showA1 :: Show A where
  show A = "Instance 1"

instance showA2 :: Show A where
  show A = "Instance 2"

main = do
  assert $ show A == "Instance 1"
  log "Done"
