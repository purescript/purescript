-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Supply
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Fresh variable supply
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply where

import Data.Functor.Identity

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a } 
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MonadWriter w, MonadReader r)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

evalSupply :: Integer -> Supply a -> a
evalSupply n = runIdentity . evalSupplyT n