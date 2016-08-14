{-# LANGUAGE TemplateHaskell #-}
-- |
-- Metadata annotations for core functional representation
--
module Language.PureScript.CoreFn.Meta where

import qualified Data.Aeson.TH as A

import Prelude.Compat

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
  | IsForeign deriving (Show, Eq)

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
  | SumType deriving (Show, Eq)

$(A.deriveJSON A.defaultOptions ''Meta)
$(A.deriveJSON A.defaultOptions ''ConstructorType)
