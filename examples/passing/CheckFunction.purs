module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test = ((\x -> x+1.0) >>> (\x -> x*2.0)) 4.0

main = log "Done"
