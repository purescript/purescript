{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Ide.Filter.Declaration
       ( IdeDeclaration(..)
       , DeclarationType(..)
       , typeDeclarationForDeclaration
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Data.Aeson
import qualified Language.PureScript.Ide.Types as PI

data DeclarationType = Value
  | Type
  | Synonym
  | DataConstructor
  | TypeClass
  | ValueOperator
  | TypeOperator
  | Kind
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
      _                   -> mzero

newtype IdeDeclaration = IdeDeclaration
  { declarationtype :: DeclarationType
  } deriving (Show, Eq, Ord)

instance FromJSON IdeDeclaration where
  parseJSON (Object o) =
    IdeDeclaration <$> o .: "declarationtype"
  parseJSON _ = mzero

typeDeclarationForDeclaration :: PI.IdeDeclaration -> IdeDeclaration
typeDeclarationForDeclaration decl = case decl of
  PI.IdeDeclValue _ -> IdeDeclaration Value
  PI.IdeDeclType _ -> IdeDeclaration Type
  PI.IdeDeclTypeSynonym _ -> IdeDeclaration Synonym
  PI.IdeDeclDataConstructor _ -> IdeDeclaration DataConstructor
  PI.IdeDeclTypeClass _ -> IdeDeclaration TypeClass
  PI.IdeDeclValueOperator _ -> IdeDeclaration ValueOperator
  PI.IdeDeclTypeOperator _ -> IdeDeclaration TypeOperator
  PI.IdeDeclKind _ -> IdeDeclaration Kind
