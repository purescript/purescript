module Main (thing, main, module A) where

import A

thing :: Int
thing = 2

main = Control.Monad.Eff.Console.log "Done"
