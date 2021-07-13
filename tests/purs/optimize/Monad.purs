module Main where

import Control.Applicative (class Applicative, pure)
import Control.Bind (class Bind, bind)

class (Applicative m, Bind m) <= Monad m

liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
liftM1 f a = do
  a' <- a
  pure (f a')

ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  pure (f' a')
