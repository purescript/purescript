module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assertThrows)

fn :: Number -> Number
fn 0.0 = 0.0
fn 1.0 = 2.0

main = do
  assertThrows $ \_ -> fn 2.0
  log "Done"
