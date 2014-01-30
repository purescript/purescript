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

module Language.PureScript.Kinds where

import Data.Data

import Control.Monad.Unify (TypedUnknown(..))

-- |
-- The data type of kinds
--
data Kind
  -- |
  -- Unification variable of type Kind
  --
  = KUnknown (TypedUnknown Kind)
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
  | FunKind Kind Kind deriving (Show, Eq, Data, Typeable)
