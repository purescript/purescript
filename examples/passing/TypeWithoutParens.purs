module Main where

import Lib (X, Y)
import Effect.Console (log)

idX :: X -> X
idX x = x

idY :: Y -> Y
idY y = y

main = log "Done"
