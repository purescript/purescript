module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data A = A B

type B = A

main = log "Done"
