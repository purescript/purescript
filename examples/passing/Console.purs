module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

replicateM_ :: forall m a. Monad m => Number -> m a -> m Unit
replicateM_ 0.0 _ = pure unit
replicateM_ n act = do
  _ <- act
  replicateM_ (n - 1.0) act

main = do
  replicateM_ 10.0 (log "Hello World!")
  log "Done"
