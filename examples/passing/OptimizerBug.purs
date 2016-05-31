module Main where

import Prelude
import Control.Monad.Eff.Console (log)

x a = 1.0 + y a

y a = x a

main = log "Done"
