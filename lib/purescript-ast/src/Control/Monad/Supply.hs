-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import "base-compat" Prelude.Compat

import "base" Control.Applicative
import "mtl" Control.Monad.Error.Class (MonadError(..))
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

import "base" Data.Functor.Identity

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

evalSupply :: Integer -> Supply a -> a
evalSupply n = runIdentity . evalSupplyT n
