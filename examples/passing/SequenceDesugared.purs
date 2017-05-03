module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

data List a = Cons a (List a) | Nil

data Sequence t = Sequence (forall m a. Monad m => t (m a) -> m (t a))

sequence :: forall t. Sequence t -> (forall m a. Monad m => t (m a) -> m (t a))
sequence (Sequence s) = s

sequenceListSeq :: forall m a. Monad m => List (m a) -> m (List a)
sequenceListSeq Nil = pure Nil
sequenceListSeq (Cons x xs) = Cons <$> x <*> sequenceListSeq xs

sequenceList :: Sequence List
sequenceList = Sequence (sequenceListSeq)

sequenceList' :: Sequence List
sequenceList' = Sequence ((\val -> case val of
  Nil -> pure Nil
  Cons x xs -> Cons <$> x <*> sequence sequenceList' xs))

sequenceList'' :: Sequence List
sequenceList'' = Sequence (sequenceListSeq :: forall m a. Monad m => List (m a) -> m (List a))

sequenceList''' :: Sequence List
sequenceList''' = Sequence ((\val -> case val of
  Nil -> pure Nil
  Cons x xs -> Cons <$> x <*> sequence sequenceList''' xs) :: forall m a. Monad m => List (m a) -> m (List a))

main = do
  void $ sequence sequenceList $ Cons (log "Done") Nil
  void $ sequence sequenceList' $ Cons (log "Done") Nil
  void $ sequence sequenceList'' $ Cons (log "Done") Nil
  void $ sequence sequenceList''' $ Cons (log "Done") Nil
