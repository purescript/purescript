module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

-- Until the functional dependency gets added to purescript-eff, 
-- we need this here.
class Monad m <= MonadEff eff m | m -> eff where
  liftEff :: forall a. Eff eff a -> m a

instance monadEffEff :: MonadEff eff (Eff eff) where
  liftEff = id

-- This should generate a warning with the correct inferred type.
test :: forall m. MonadEff _ m => m Unit
test = liftEff $ log "Done"

test1 :: Eff _ Unit
test1 = liftEff $ log "Done"

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = test
