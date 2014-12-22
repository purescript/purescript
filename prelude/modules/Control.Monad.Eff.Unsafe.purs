module Control.Monad.Eff.Unsafe where

import Control.Monad.Eff

foreign import unsafeInterleaveEff """
  function unsafeInterleaveEff(f) {
    return f;
  }""" :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a