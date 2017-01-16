module Main where

import Control.Monad.Eff.Console (log)

data X = X

f a@X X = a

main = log "Done"
