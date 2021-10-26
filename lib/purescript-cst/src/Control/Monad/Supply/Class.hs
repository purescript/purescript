-- |
-- A class for monads supporting a supply of fresh names
--

module Control.Monad.Supply.Class where

import Prelude.Compat

import Control.Monad.Supply
import Control.Monad.State
import Control.Monad.Writer
import Data.Text (Text, pack)

class Monad m => MonadSupply m where
  -- |
  -- Get a new unique Integer
  fresh :: m Integer
  -- |
  -- See what the next unique Integer will be
  peek :: m Integer
  default fresh :: (MonadTrans t, MonadSupply n, m ~ t n) => m Integer
  fresh = lift fresh
  default peek :: (MonadTrans t, MonadSupply n, m ~ t n) => m Integer
  peek = lift peek

instance Monad m => MonadSupply (SupplyT m) where
  fresh = SupplyT $ do
    n <- get
    put (n + 1)
    return n
  peek = SupplyT get

instance MonadSupply m => MonadSupply (StateT s m)
instance (Monoid w, MonadSupply m) => MonadSupply (WriterT w m)

-- | Convenience function for returning "$x" where `x` is the next unique integer
freshName :: MonadSupply m => m Text
freshName = fmap (("$" <> ) . pack . show) fresh
