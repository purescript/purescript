module Main where

import Prelude
import Control.Monad.Eff.Console (log)

fib = \n -> case n of
  0.0 -> 1.0
  1.0 -> 1.0
  n -> fib (n - 1.0) + fib (n - 2.0)

main = log "Done"
