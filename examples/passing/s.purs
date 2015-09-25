module Main where

import Prelude

s = \x y z -> x z (y z)

main = Control.Monad.Eff.Console.log "Done"
