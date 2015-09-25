module Main where

import Prelude

f x = g x
  where
  g x = go x
    where
    go x = go1 (x - 1.0)
    go1 x = go x

main = Control.Monad.Eff.Console.log "Done"
