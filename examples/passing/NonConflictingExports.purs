-- No failure here as the export `thing` only refers to Main.thing
module Main (thing, main) where

import A
import Control.Monad.Eff.Console (log)

thing :: Int
thing = 2

main = log "Done"
