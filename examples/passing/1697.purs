module Main where

import Prelude
import Control.Monad.Eff.Console (log)

_2 :: forall a. a -> a
_2 a = a

x :: forall m. Monad m => m Unit
x = do
  _ <- pure unit
  pure unit

y :: forall m. Monad m => m Unit
y = do
  _ <- pure unit
  pure unit

wtf :: forall m. Monad m => m Unit
wtf = do
  _ <- pure unit
  let tmp = _2 1
  pure unit

main = log "Done"
