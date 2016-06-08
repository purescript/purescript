module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data One a = One a

one' (One a) = a

main = log "Done"
