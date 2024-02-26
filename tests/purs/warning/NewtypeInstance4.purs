-- @shouldWarnWith UnverifiableSuperclassInstance
module Main where

import Prelude
import Data.Tuple (Tuple(..))

class Monoid w <= MonadTell w m where
  tell :: w -> m Unit

class (MonadTell w m) <= MonadWriter w m where
  listen :: forall a. m a -> m (Tuple w a)

instance monadTellTuple :: forall w. Monoid w => MonadTell w (Tuple w) where
  tell w = Tuple w unit

instance monadWriterTuple :: forall w. Monoid w => MonadWriter w (Tuple w) where
  listen (Tuple w a) = Tuple w (Tuple w a)

newtype MyWriter w a = MyWriter (Tuple w a)

-- No fundep means this is unverifiable
derive newtype instance monadTellMyWriter :: forall w. Monoid w => MonadTell w (MyWriter w)
derive newtype instance monadWriterMyWriter :: forall w. Monoid w => MonadWriter w (MyWriter w)
