-- @shouldWarnWith MissingNewtypeSuperclassInstance
module Main where

import Prelude
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(..))

class (Monad m, Monoid w) <= MonadWriter w m | m -> w where
  tell :: w -> m Unit

instance monadWriterTuple :: Monoid w => MonadWriter w (Tuple w) where
  tell w = Tuple w unit

newtype MyWriter w a = MyWriter (Tuple w a)

derive newtype instance monadWriterMyWriter :: Monoid w => MonadWriter w (MyWriter w)
