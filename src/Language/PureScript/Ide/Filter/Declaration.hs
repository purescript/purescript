{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Ide.Filter.Declaration
       ( DeclarationType(..)
       , declarationType
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Data.Aeson
import qualified Language.PureScript.Ide.Types as PI

data DeclarationType
  = Value
  | Type
  | Synonym
  | DataConstructor
  | TypeClass
  | ValueOperator
  | TypeOperator
  | Kind
  | Module
  deriving (Show, Eq, Ord)

instance FromJSON DeclarationType where
  parseJSON = withText "Declaration type tag" $ \str ->
    case str of
      "value"             -> pure Value
      "type"              -> pure Type
      "synonym"           -> pure Synonym
      "dataconstructor"   -> pure DataConstructor
      "typeclass"         -> pure TypeClass
      "valueoperator"     -> pure ValueOperator
      "typeoperator"      -> pure TypeOperator
      "kind"              -> pure Kind
      "module"            -> pure Module
      _                   -> mzero

declarationType :: PI.IdeDeclaration -> DeclarationType
declarationType decl = case decl of
  PI.IdeDeclValue _ -> Value
  PI.IdeDeclType _ -> Type
  PI.IdeDeclTypeSynonym _ -> Synonym
  PI.IdeDeclDataConstructor _ -> DataConstructor
  PI.IdeDeclTypeClass _ -> TypeClass
  PI.IdeDeclValueOperator _ -> ValueOperator
  PI.IdeDeclTypeOperator _ -> TypeOperator
  PI.IdeDeclKind _ -> Kind
  PI.IdeDeclModule _ -> Module
