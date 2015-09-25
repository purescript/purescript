-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Traversals
-- Copyright   :  (c) 2014 Phil Freeman
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Common functions for implementing generic traversals
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.PureScript.Traversals where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

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

