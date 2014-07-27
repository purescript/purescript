module Main where

import Debug.Trace
import Control.Monad.Eff

newtype T a = T (Eff (trace :: Trace) a)

runT :: forall a. T a -> Eff (trace :: Trace) a
runT (T t) = t

instance functorT :: Functor T where
  (<$>) f (T t) = T (f <$> t)

instance applyT :: Apply T where
  (<*>) (T f) (T x) = T (f <*> x)

instance applicativeT :: Applicative T where
  pure t = T (pure t)

instance bindT :: Bind T where
  (>>=) (T t) f = T (t >>= \x -> runT (f x))

instance monadT :: Monad T

main = runT do
  T $ trace "Done"
  T $ trace "Done"
  T $ trace "Done"
