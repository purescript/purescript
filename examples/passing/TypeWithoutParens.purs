module Main where

import Lib (X, Y)
import Control.Monad.Eff.Console (log)

idX :: X -> X
idX x = x

idY :: Y -> Y
idY y = y

main = log "Done"
