module Main where

-- qualified import with qualified imported names
import qualified Control.Monad.Eff.Console (log) as Console

main = Console.log "Success!"
