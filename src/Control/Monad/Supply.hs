-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader, MonadTrans)
import Control.Monad (MonadPlus)
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Writer (MonadWriter)
import Data.Int (Int64)

import Data.Functor.Identity (Identity(..))

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Int64 m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus)

runSupplyT :: Int64 -> SupplyT m a -> m (a, Int64)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Int64 -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Int64 -> Supply a -> (a, Int64)
runSupply n = runIdentity . runSupplyT n
