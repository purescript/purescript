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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Supply where

import Data.Functor.Identity

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a } deriving (Functor, Applicative, Monad, MonadTrans)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

evalSupplyT :: (Functor m) => Integer -> SupplyT m a -> m a
evalSupplyT n = fmap fst . runSupplyT n

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

evalSupply :: Integer -> Supply a -> a
evalSupply n = runIdentity . evalSupplyT n

instance (MonadError e m) => MonadError e (SupplyT m) where
  throwError = SupplyT . throwError
  catchError e f = SupplyT $ catchError (unSupplyT e) (unSupplyT . f)
