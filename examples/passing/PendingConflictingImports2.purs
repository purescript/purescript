module Main where

import A
import Control.Monad.Eff.Console (log)

-- No error as we never force `thing` to be resolved in `Main`
thing :: Int
thing = 2

main = log "Done"
