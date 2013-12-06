-----------------------------------------------------------------------------
--
-- Module      :  Data.Generics.Extras
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Data.Generics.Extras where

import Data.Data

everywhereM' :: (Monad m, Data d) => (forall d1. (Data d1) => d1 -> m d1) -> d -> m d
everywhereM' f x = do
  y <- f x
  gmapM (everywhereM' f) y
