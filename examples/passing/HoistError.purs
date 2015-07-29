module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Test.Assert

main = do
  let x = 0.0
  assert $ x == 0.0
  let x = 1.0 + 1.0
  log "Done"
