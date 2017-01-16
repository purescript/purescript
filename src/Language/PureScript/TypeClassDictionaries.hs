{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.PureScript.TypeClassDictionaries where

import Prelude.Compat

import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope v
  = TypeClassDictionaryInScope {
    -- | The value with which the dictionary can be accessed at runtime
      tcdValue :: v
    -- | How to obtain this instance via superclass relationships
    , tcdPath :: [(Qualified (ProperName 'ClassName), Integer)]
    -- | The name of the type class to which this type class instance applies
    , tcdClassName :: Qualified (ProperName 'ClassName)
    -- | The types to which this type class instance applies
    , tcdInstanceTypes :: [Type]
    -- | Type class dependencies which must be satisfied to construct this dictionary
    , tcdDependencies :: Maybe [Constraint]
    }
    deriving (Show, Functor, Foldable, Traversable)

type NamedDict = TypeClassDictionaryInScope (Qualified Ident)

