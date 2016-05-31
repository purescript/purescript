module Main where

import Prelude
import Control.Monad.Eff.Console (log)

s = \x y z -> x z (y z)

main = log "Done"
