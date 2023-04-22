-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadPlus, MonadReader, MonadTrans)
import Control.Monad.State (StateT(..))
import Control.Monad.Writer (MonadWriter)

import Data.Functor.Identity (Identity(..))

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n
