module Main where

import Control.Monad.Eff.Console
import Prim (Int)

type Number = Int

z :: Number
z = 0

main = log "Done"
