module Main where

import M1 (X(..))
import Effect.Console (log)

testX :: X
testX = X
testY = Y

main = log "Done"
