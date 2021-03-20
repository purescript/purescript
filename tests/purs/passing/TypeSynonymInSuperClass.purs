module Main where

import Prelude
import Effect.Console (log)

type Env = { foo :: String }

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

class (Monad m, MonadAsk Env m) <= MonadAskEnv m

test :: forall m. MonadAskEnv m => m Boolean
test = do
  { foo } <- ask
  pure (foo == "test")

main = log "Done"
