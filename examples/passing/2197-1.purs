module Main where

import Control.Monad.Eff.Console
import Prim as P

type Number = P.Number
type Test = {}

z :: Number
z = 0.0

main = log "Done"
