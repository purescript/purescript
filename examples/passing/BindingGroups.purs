module Main where

import Prelude

foo = bar
  where bar r = r + 1.0

r = foo 2.0

main = Control.Monad.Eff.Console.log "Done"
