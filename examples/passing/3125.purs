module Main where

import Prelude
import Data.Monoid (class Monoid, mempty)
import Control.Monad.Eff.Console (log, logShow)

data B a = B a a

memptyB :: forall a b. Monoid b => B (a -> b)
memptyB = B l r where
  l _ = mempty
  r _ = mempty

main = do
  logShow $ case (memptyB :: B (Int -> Array Unit)) of B l r -> l 0 == r 0
  log "Done"
