module Main where

import Prelude

data A = A B

type B = A

main = Control.Monad.Eff.Console.log "Done"
