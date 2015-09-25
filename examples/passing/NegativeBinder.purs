module Main where

import Prelude

test :: Number -> Boolean
test -1.0 = false
test _  = true

main = Control.Monad.Eff.Console.log "Done"
