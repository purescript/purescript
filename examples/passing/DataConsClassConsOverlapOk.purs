module Main where

import Control.Monad.Eff.Console (log)
import Prim.Row (class Cons)

data Cons = Cons

main = log "Done"
