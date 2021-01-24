{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Ide.Filter.Declaration
       ( DeclarationType(..)
       , declarationType
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Control.Monad.Fail (fail)
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
      "module"            -> pure Module
      s                   -> fail ("Unknown declaration type: " <> show s)

declarationType :: PI.IdeDeclaration -> DeclarationType
declarationType decl = case decl of
  PI.IdeDeclValue _ -> Value
  PI.IdeDeclType _ -> Type
  PI.IdeDeclTypeSynonym _ -> Synonym
  PI.IdeDeclDataConstructor _ -> DataConstructor
  PI.IdeDeclTypeClass _ -> TypeClass
  PI.IdeDeclValueOperator _ -> ValueOperator
  PI.IdeDeclTypeOperator _ -> TypeOperator
  PI.IdeDeclModule _ -> Module
