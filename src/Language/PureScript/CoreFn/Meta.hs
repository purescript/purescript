-- |
-- Metadata annotations for core functional representation
--
module Language.PureScript.CoreFn.Meta where

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
  | IsForeign
  -- |
  -- The contained value is a where clause
  --
  | IsWhere
  deriving (Show, Eq, Ord)

-- |
-- Data constructor metadata
--
data ConstructorType
  -- |
  -- The constructor is for a type with a single constructor
  --
  = ProductType
  -- |
  -- The constructor is for a type with multiple constructors
  --
  | SumType deriving (Show, Eq, Ord)
