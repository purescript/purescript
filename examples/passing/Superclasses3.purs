module Main where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff

class (Monad m) <= MonadWriter w m where
  tell :: w -> m Unit

testFunctor :: forall m. (Monad m) => m Number -> m Number
testFunctor n = (+) 1.0 <$> n

test :: forall w m. (Monad m, MonadWriter w m) => w -> m Unit
test w = do
  tell w
  tell w
  tell w

data MTrace a = MTrace (Eff (console :: CONSOLE) a)

runMTrace :: forall a. MTrace a -> Eff (console :: CONSOLE) a
runMTrace (MTrace a) = a

instance functorMTrace :: Functor MTrace where
  map = liftM1

instance applyMTrace :: Apply MTrace where
  apply = ap

instance applicativeMTrace :: Applicative MTrace where
  pure = MTrace <<< return

instance bindMTrace :: Bind MTrace where
  bind m f = MTrace (runMTrace m >>= (runMTrace <<< f))

instance monadMTrace :: Monad MTrace

instance writerMTrace :: MonadWriter String MTrace where
  tell s = MTrace (log s)

main = runMTrace $ test "Done"
