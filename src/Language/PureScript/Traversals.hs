-- | Common functions for implementing generic traversals
module Language.PureScript.Traversals where

import Prelude

sndM :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
sndM f (a, b) = (a, ) <$> f b

sndM' :: (Functor f) => (a -> b -> f c) -> (a, b) -> f (a, c)
sndM' f (a, b) = (a, ) <$> f a b

thirdM :: (Functor f) => (c -> f d) -> (a, b, c) -> f (a, b, d)
thirdM f (a, b, c) = (a, b, ) <$> f c

pairM :: (Applicative f) => (a -> f c) -> (b -> f d) -> (a, b) -> f (c, d)
pairM f g (a, b)  = (,) <$> f a <*> g b

eitherM :: (Applicative f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
eitherM f _ (Left a)  = Left  <$> f a
eitherM _ g (Right b) = Right <$> g b

defS :: (Monad m) => st -> val -> m (st, val)
defS s val = return (s, val)
