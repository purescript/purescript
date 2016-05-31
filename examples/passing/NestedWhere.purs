module Main where

import Prelude
import Control.Monad.Eff.Console (log)

f x = g x
  where
  g x = go x
    where
    go x = go1 (x - 1.0)
    go1 x = go x

main = log "Done"
