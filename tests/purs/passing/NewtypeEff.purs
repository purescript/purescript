module Main where

import Prelude
import Effect.Console
import Effect

newtype T a = T (Effect a)

runT :: forall a. T a -> Effect a
runT (T t) = t

instance functorT :: Functor T where
  map f (T t) = T (f <$> t)

instance applyT :: Apply T where
  apply (T f) (T x) = T (f <*> x)

instance applicativeT :: Applicative T where
  pure t = T (pure t)

instance bindT :: Bind T where
  bind (T t) f = T (t >>= \x -> runT (f x))

instance monadT :: Monad T

main = runT do
  T $ log "Done"
  T $ log "Done"
  T $ log "Done"
