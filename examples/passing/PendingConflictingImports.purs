module Main where

-- No error as we never force `thing` to be resolved in `Main`
import A
import B
import Control.Monad.Eff.Console (log)

main = log "Done"
