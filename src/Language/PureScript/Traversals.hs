-- | Common functions for implementing generic traversals
module Language.PureScript.Traversals where

import Prelude.Compat

fstM :: (Functor f) => (a -> f c) -> (a, b) -> f (c, b)
fstM f (a, b) = flip (,) b <$> f a

sndM :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
sndM f (a, b) = (,) a <$> f b

thirdM :: (Functor f) => (c -> f d) -> (a, b, c) -> f (a, b, d)
thirdM f (a, b, c) = (,,) a b <$> f c

pairM :: (Applicative f) => (a -> f c) -> (b -> f d) -> (a, b) -> f (c, d)
pairM f g (a, b)  = (,) <$> f a <*> g b

maybeM :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
maybeM _ Nothing = pure Nothing
maybeM f (Just a) = Just <$> f a

eitherM :: (Applicative f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
eitherM f _ (Left a)  = Left  <$> f a
eitherM _ g (Right b) = Right <$> g b

defS :: (Monad m) => st -> val -> m (st, val)
defS s val = return (s, val)
