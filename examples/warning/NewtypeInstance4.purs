-- @shouldWarnWith UnverifiableSuperclassInstance
module Main where

import Prelude
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(..))

class Monoid w <= MonadTell w m where
  tell :: w -> m Unit

class (MonadTell w m) <= MonadWriter w m where
  listen :: forall a. m a -> m (Tuple w a)

instance monadTellTuple :: Monoid w => MonadTell w (Tuple w) where
  tell w = Tuple w unit

instance monadWriterTuple :: Monoid w => MonadWriter w (Tuple w) where
  listen (Tuple w a) = Tuple w (Tuple w a)

newtype MyWriter w a = MyWriter (Tuple w a)

-- No fundep means this is unverifiable
derive newtype instance monadTellMyWriter :: Monoid w => MonadTell w (MyWriter w)
derive newtype instance monadWriterMyWriter :: Monoid w => MonadWriter w (MyWriter w)
