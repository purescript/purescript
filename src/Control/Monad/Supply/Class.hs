-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Supply.Class
-- Copyright   :  (c) PureScript 2015
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A class for monads supporting a supply of fresh names
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Supply.Class where

import Control.Monad.Supply
import Control.Monad.State

class (Monad m) => MonadSupply m where
  fresh :: m Integer
  
instance (Monad m) => MonadSupply (SupplyT m) where
  fresh = SupplyT $ do
    n <- get
    put (n + 1)
    return n
  
instance (MonadSupply m) => MonadSupply (StateT s m) where
  fresh = lift fresh

freshName :: (MonadSupply m) => m String
freshName = liftM (('_' :) . show) fresh
