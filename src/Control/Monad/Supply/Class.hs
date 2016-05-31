-- |
-- A class for monads supporting a supply of fresh names
--
module Control.Monad.Supply.Class where

import Prelude.Compat

import Control.Monad.Supply
import Control.Monad.State

class Monad m => MonadSupply m where
  fresh :: m Integer

instance Monad m => MonadSupply (SupplyT m) where
  fresh = SupplyT $ do
    n <- get
    put (n + 1)
    return n

instance MonadSupply m => MonadSupply (StateT s m) where
  fresh = lift fresh

freshName :: MonadSupply m => m String
freshName = fmap (('$' :) . show) fresh
