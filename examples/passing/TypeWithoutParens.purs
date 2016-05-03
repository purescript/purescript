module Main where

import Control.Monad.Eff.Console (log)
import Lib (X, Y)

idX :: X -> X
idX x = x

idY :: Y -> Y
idY y = y

main = log "Done"
