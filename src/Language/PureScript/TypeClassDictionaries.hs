module Language.PureScript.TypeClassDictionaries where

import Prelude.Compat

import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope
  = TypeClassDictionaryInScope {
    -- | The identifier with which the dictionary can be accessed at runtime
      tcdName :: Qualified Ident
    -- | How to obtain this instance via superclass relationships
    , tcdPath :: [Integer]
    -- | The name of the type class to which this type class instance applies
    , tcdClassName :: Type
    -- | The types to which this type class instance applies
    , tcdInstanceTypes :: [Type]
    -- | Type class dependencies which must be satisfied to construct this dictionary
    , tcdDependencies :: Maybe [(Qualified (ProperName 'ClassName), [Type])]
    }
    deriving (Show, Read)

-- |
-- A simplified representation of expressions which are used to represent type
-- class dictionaries at runtime, which can be compared for equality
--
data DictionaryValue
  -- |
  -- A dictionary which is brought into scope by a local constraint
  --
  = LocalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which is brought into scope by an instance declaration
  --
  | GlobalDictionaryValue (Qualified Ident)
  -- |
  -- A dictionary which depends on other dictionaries
  --
  | DependentDictionaryValue (Qualified Ident) [DictionaryValue]
  -- |
  -- A subclass dictionary
  --
  | SubclassDictionaryValue DictionaryValue Integer
  deriving (Show, Read, Ord, Eq)
