module Main where

import Prelude
import Effect
import Effect.Console

fn :: Partial => Number -> Number
fn 0.0 = 0.0
fn 1.0 = 2.0

main = log "Done"
