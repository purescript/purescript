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

module Language.PureScript.Kinds where

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
  | FunKind Kind Kind deriving (Show, Eq)

everywhereOnKinds :: (Kind -> Kind) -> Kind -> Kind
everywhereOnKinds f = go
  where
  go (Row k1) = f (Row (go k1))
  go (FunKind k1 k2) = f (FunKind (go k1) (go k2))
  go other = f other

everythingOnKinds :: (r -> r -> r) -> (Kind -> r) -> Kind -> r
everythingOnKinds (<>) f = go
  where
  go k@(Row k1) = f k <> go k1
  go k@(FunKind k1 k2) = f k <> go k1 <> go k2
  go other = f other
