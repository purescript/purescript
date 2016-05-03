module Main where

import M1 (X(..))

testX :: X
testX = X
testY = Y

main = Control.Monad.Eff.Console.log "Done"
