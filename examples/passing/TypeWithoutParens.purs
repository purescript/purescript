module Main where

import Lib (X, Y)

idX :: X -> X
idX x = x

idY :: Y -> Y
idY y = y

main = Control.Monad.Eff.Console.log "Done"
