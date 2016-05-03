module Main where

-- No error as we never force `thing` to be resolved in `Main`
import A
import B

main = Control.Monad.Eff.Console.log "Done"
