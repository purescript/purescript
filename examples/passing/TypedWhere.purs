module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data E a b = L a | R b

data L a = C a (L a) | N

lefts :: forall a b. L (E a b) -> L a
lefts = go N
  where
  go :: forall a b. L a -> L (E a b) -> L a
  go ls N = ls
  go ls (C (L a) rest) = go (C a ls) rest
  go ls (C _ rest) = go ls rest

main = log "Done"
