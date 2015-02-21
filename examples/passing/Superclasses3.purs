module Main where

import Debug.Trace

import Control.Monad.Eff

class (Monad m) <= MonadWriter w m where
  tell :: w -> m Unit

testFunctor :: forall m. (Monad m) => m Number -> m Number
testFunctor n = (+) 1 <$> n

test :: forall w m. (Monad m, MonadWriter w m) => w -> m Unit
test w = do
  tell w
  tell w
  tell w

data MTrace a = MTrace (Eff (trace :: Trace) a)

runMTrace :: forall a. MTrace a -> Eff (trace :: Trace) a
runMTrace (MTrace a) = a

instance functorMTrace :: Functor MTrace where
  (<$>) = liftM1

instance applyMTrace :: Apply MTrace where
  (<*>) = ap

instance applicativeMTrace :: Applicative MTrace where
  pure = MTrace <<< return

instance bindMTrace :: Bind MTrace where
  (>>=) m f = MTrace (runMTrace m >>= (runMTrace <<< f))

instance monadMTrace :: Monad MTrace

instance writerMTrace :: MonadWriter String MTrace where
  tell s = MTrace (trace s)

main = runMTrace $ test "Done"
