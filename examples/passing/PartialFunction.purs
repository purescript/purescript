module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

fn :: Partial => Number -> Number
fn 0.0 = 0.0
fn 1.0 = 2.0

main = log "Done"
