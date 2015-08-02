module Main where

import Prelude

test = ((\x -> x+1.0) >>> (\x -> x*2.0)) 4.0

main = Control.Monad.Eff.Console.log "Done"
