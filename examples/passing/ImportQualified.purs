module Main where

import Prelude
import Control.Monad.Eff
import M1
import Control.Monad.Eff.Console as C

main = C.log (log "Done")
