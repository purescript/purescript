module Main where

import Control.Monad.Eff.Console (log)
import M1 (X(..))

testX :: X
testX = X
testY = Y

main = log "Done"
