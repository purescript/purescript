module Main where

import Prelude
import Effect.Console (log)

data One a = One a

one' (One a) = a

main = log "Done"
