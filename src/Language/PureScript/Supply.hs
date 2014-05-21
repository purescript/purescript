-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Supply
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

module Language.PureScript.Supply where

import Data.Functor.Identity

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error.Class

newtype SupplyT m a = SupplyT { unSupplyT :: StateT Integer m a } deriving (Functor, Applicative, Monad, MonadTrans)

runSupplyT :: Integer -> SupplyT m a -> m (a, Integer)
runSupplyT n = flip runStateT n . unSupplyT

type Supply = SupplyT Identity

runSupply :: Integer -> Supply a -> (a, Integer)
runSupply n = runIdentity . runSupplyT n

fresh :: (Monad m) => SupplyT m Integer
fresh = SupplyT $ do
  n <- get
  put (n + 1)
  return n

freshName :: (Functor m, Monad m) => SupplyT m String
freshName = ('_' :) . show <$> fresh

instance (MonadError e m) => MonadError e (SupplyT m) where
  throwError = SupplyT . throwError
  catchError e f = SupplyT $ catchError (unSupplyT e) (unSupplyT . f)
