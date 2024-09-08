-- @shouldWarnWith MissingNewtypeSuperclassInstance
module Main where

import Prelude
import Data.Tuple (Tuple(..))

class (Monad m, Monoid w) <= MonadTell w m | m -> w where
  tell :: w -> m Unit

class (MonadTell w m) <= MonadWriter w m | m -> w where
  listen :: forall a. m a -> m (Tuple w a)

instance monadTellTuple :: forall w. Monoid w => MonadTell w (Tuple w) where
  tell w = Tuple w unit

instance monadWriterTuple :: forall w. Monoid w => MonadWriter w (Tuple w) where
  listen (Tuple w a) = Tuple w (Tuple w a)

newtype MyWriter w a = MyWriter (Tuple w a)

derive newtype instance monadWriterMyWriter :: forall w. Monoid w => MonadWriter w (MyWriter w)
