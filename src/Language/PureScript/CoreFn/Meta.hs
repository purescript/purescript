-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Meta
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | Metadata annotations for core functional representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CoreFn.Meta where

import qualified Data.Data as D

import Language.PureScript.Names

-- |
-- Metadata annotations
--
data Meta
  -- |
  -- The contained value is a data constructor
  --
  = IsConstructor ConstructorType [Ident]
  -- |
  -- The contained value is a newtype
  --
  | IsNewtype
  -- |
  -- The contained value is a typeclass dictionary constructor
  --
  | IsTypeClassConstructor
  -- |
  -- The contained reference is for a foreign member
  --
  | IsForeign deriving (Show, Read, D.Data, D.Typeable)

-- |
-- Data constructor metadata
--
data ConstructorType
  -- |
  -- The constructor is for a type with a single construcor
  --
  = ProductType
  -- |
  -- The constructor is for a type with multiple construcors
  --
  | SumType deriving (Show, Read, D.Data, D.Typeable)
