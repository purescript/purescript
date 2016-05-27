module Main where

import M1 (X(..))
import Control.Monad.Eff.Console (log)

testX :: X
testX = X
testY = Y

main = log "Done"
