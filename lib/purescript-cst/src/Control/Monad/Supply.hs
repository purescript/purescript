-- |
-- Fresh variable supply
--
module Control.Monad.Supply where

import Prelude.Compat

import Control.Applicative
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r, Alternative, MonadPlus)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n
