module Main where

import Control.Monad.Eff

data Sequence t = Sequence (forall m a. (Monad m) => t (m a) -> m (t a))

sequence :: forall t. Sequence t -> (forall m a. (Monad m) => t (m a) -> m (t a))
sequence (Sequence s) = s

sequenceArraySeq :: forall m a. (Monad m) => Array (m a) -> m (Array a)
sequenceArraySeq [] = pure []
sequenceArraySeq (x:xs) = (:) <$> x <*> sequenceArraySeq xs

sequenceArray :: Sequence []
sequenceArray = Sequence (sequenceArraySeq)

sequenceArray' :: Sequence []
sequenceArray' = Sequence ((\val -> case val of
  [] -> pure []
  (x:xs) -> (:) <$> x <*> sequence sequenceArray' xs))

sequenceArray'' :: Sequence []
sequenceArray'' = Sequence (sequenceArraySeq :: forall m a. (Monad m) => Array (m a) -> m (Array a))

sequenceArray''' :: Sequence []
sequenceArray''' = Sequence ((\val -> case val of
  [] -> pure []
  (x:xs) -> (:) <$> x <*> sequence sequenceArray''' xs) :: forall m a. (Monad m) => Array (m a) -> m (Array a))

main = do
  sequence sequenceArray $ [Debug.Trace.trace "Done"]
  sequence sequenceArray' $ [Debug.Trace.trace "Done"]
  sequence sequenceArray'' $ [Debug.Trace.trace "Done"]
  sequence sequenceArray''' $ [Debug.Trace.trace "Done"]
