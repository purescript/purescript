module Main (thing, main, module A) where

import A
import Control.Monad.Eff.Console (log)

thing :: Int
thing = 2

main = log "Done"
