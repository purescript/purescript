{-# LANGUAGE UndecidableInstances #-}
-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader, MonadTrans)
import Control.Monad (MonadPlus)
import Control.Monad.State (StateT(..), MonadState(..))
import Control.Monad.Writer (MonadWriter)

import Data.Functor.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus, MonadIO)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

instance MonadState s m => MonadState s (SupplyT m) where
  get = lift get
  put = lift . put
