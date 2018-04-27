module Main where

import Prelude
import Effect.Console (log)

data A = A B

type B = A

main = log "Done"
