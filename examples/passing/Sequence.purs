module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

data List a = Cons a (List a) | Nil

class Sequence t where
  sequence :: forall m a. (Monad m) => t (m a) -> m (t a)

instance sequenceList :: Sequence List where
  sequence Nil = pure Nil
  sequence (Cons x xs) = Cons <$> x <*> sequence xs

main = sequence $ Cons (log "Done") Nil
