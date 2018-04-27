module Main where

import Prelude
import Effect
import Effect.Console

data Identity a = Identity a

newtype IdentityEff a = IdentityEff (Effect (Identity a))

test :: forall a. IdentityEff a -> IdentityEff Unit
test (IdentityEff action) = IdentityEff $ do
  (Identity x :: Identity _) <- action
  pure $ Identity unit

main = log "Done"
