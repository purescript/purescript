module Main where

import Prelude

x a = 1.0 + y a

y a = x a

main = Control.Monad.Eff.Console.log "Done"
