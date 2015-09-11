-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Kinds
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Kinds where

import Data.Data
import qualified Data.Aeson.TH as A

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Unify (Unknown)

-- |
-- The data type of kinds
--
data Kind
  -- |
  -- Unification variable of type Kind
  --
  = KUnknown Unknown
  -- |
  -- The kind of types
  --
  | Star
  -- |
  -- The kind of effects
  --
  | Bang
  -- |
  -- Kinds for labelled, unordered rows without duplicates
  --
  | Row Kind
  -- |
  -- Function kinds
  --
  | FunKind Kind Kind deriving (Show, Read, Eq, Ord, Data, Typeable)

$(A.deriveJSON A.defaultOptions ''Kind)

everywhereOnKinds :: (Kind -> Kind) -> Kind -> Kind
everywhereOnKinds f = go
  where
  go (Row k1) = f (Row (go k1))
  go (FunKind k1 k2) = f (FunKind (go k1) (go k2))
  go other = f other

everywhereOnKindsM :: (Functor m, Applicative m, Monad m) => (Kind -> m Kind) -> Kind -> m Kind
everywhereOnKindsM f = go
  where
  go (Row k1) = (Row <$> go k1) >>= f
  go (FunKind k1 k2) = (FunKind <$> go k1 <*> go k2) >>= f
  go other = f other

everythingOnKinds :: (r -> r -> r) -> (Kind -> r) -> Kind -> r
everythingOnKinds (<>) f = go
  where
  go k@(Row k1) = f k <> go k1
  go k@(FunKind k1 k2) = f k <> go k1 <> go k2
  go other = f other
