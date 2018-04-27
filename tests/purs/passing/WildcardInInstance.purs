module Main where

import Prelude
import Effect
import Effect.Console

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

instance monadAskFun :: MonadAsk r ((->) r) where
  ask = identity

-- This should generate a warning with the correct inferred type.
test :: forall m. MonadAsk _ m  => m Int
test = do
  x <- ask
  pure (x + 1)

main :: Effect Unit
main = do
  log "Done"
