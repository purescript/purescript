module Control.Monad.Eff.Unsafe where

import Control.Monad.Eff (Eff, runPure)
import Control.Semigroupoid ((<<<))

-- | Change the type of an effectful computation, allowing it to be run in
-- | another context.
-- |
-- | *Note*: use of this function can result in arbitrary side-effects.
foreign import unsafeInterleaveEff
  :: forall eff1 eff2 a
   . Eff eff1 a
  -> Eff eff2 a

-- | Run an effectful computation.
-- |
-- | *Note*: use of this function can result in arbitrary side-effects.
unsafePerformEff :: forall eff a. Eff eff a -> a
unsafePerformEff = runPure <<< unsafeInterleaveEff
