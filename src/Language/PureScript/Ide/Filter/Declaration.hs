{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Ide.Filter.Declaration
       ( DeclarationType(..)
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Control.Monad.Fail (fail)
import           Data.Aeson

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

instance ToJSON DeclarationType where
  toJSON dt = toJSON $ case dt of
    Value           -> "value" :: Text
    Type            -> "type"
    Synonym         -> "synonym"
    DataConstructor -> "dataconstructor"
    TypeClass       -> "typeclass"
    ValueOperator   -> "valueoperator"
    TypeOperator    -> "typeoperator"
    Module          -> "module"
