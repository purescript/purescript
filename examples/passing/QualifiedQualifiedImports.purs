module Main where

-- qualified import with qualified imported names
import Control.Monad.Eff.Console (log) as Console

main = Console.log "Done"
