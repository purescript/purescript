module Main where

import Prelude
import Effect
import Effect.Console

class Monad m <= MonadEffect m where
  liftEffect :: forall a. Effect a -> m a

instance monadEffectEffect :: MonadEffect Effect where
  liftEffect = identity

-- This should generate a warning with the correct inferred type.
test :: forall m. MonadEffect m => m Unit
test = liftEffect $ log "Done"

test1 :: Effect Unit
test1 = liftEffect $ log "Done"

main :: Effect Unit
main = test
