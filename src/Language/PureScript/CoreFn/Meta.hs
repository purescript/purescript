-- |
-- Metadata annotations for core functional representation
--
module Language.PureScript.CoreFn.Meta where

import Prelude.Compat
import Data.Text (Text)

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
  | IsTypeClassConstructor [(Text, Maybe (Qualified (ProperName 'ClassName)))]
  -- |
  -- Application of type class constructor
  | IsTypeClassConstructorApp (Qualified (ProperName 'ClassName))
  -- |
  -- The contained reference is for a foreign member
  --
  | IsForeign deriving (Show, Eq, Ord)

metaIsTypeClass :: Meta -> Bool
metaIsTypeClass (IsTypeClassConstructor _) = True
metaIsTypeClass (IsTypeClassConstructorApp _) = True
metaIsTypeClass _ = False

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
  | SumType deriving (Show, Eq, Ord)
