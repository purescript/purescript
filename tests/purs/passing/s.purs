module Main where

import Prelude
import Effect.Console (log)

s = \x y z -> x z (y z)

main = log "Done"
