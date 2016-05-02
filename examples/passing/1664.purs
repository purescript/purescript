module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

data Identity a = Identity a

newtype IdentityEff e a = IdentityEff (Eff e (Identity a))

test :: forall e a. IdentityEff e a -> IdentityEff e Unit
test (IdentityEff action) = IdentityEff $ do
  (Identity x :: Identity _) <- action
  pure $ Identity unit

main = log "Done"
