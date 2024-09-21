module Language.PureScript.Ide.Filter.Declaration
       ( DeclarationType(..)
       ) where

import Protolude                     hiding (isPrefixOf)

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple (SQLData(..))

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
  parseJSON = withText "Declaration type tag" $ \case
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
  toJSON = toJSON . \case
    Value           -> "value" :: Text
    Type            -> "type"
    Synonym         -> "synonym"
    DataConstructor -> "dataconstructor"
    TypeClass       -> "typeclass"
    ValueOperator   -> "valueoperator"
    TypeOperator    -> "typeoperator"
    Module          -> "module"

instance ToField DeclarationType where
  toField Value = SQLText "value"
  toField Type = SQLText "type"
  toField Synonym = SQLText "synonym"
  toField DataConstructor = SQLText "dataconstructor"
  toField TypeClass = SQLText "typeclass"
  toField ValueOperator = SQLText "valueoperator"
  toField TypeOperator = SQLText "typeoperator"
  toField Module = SQLText "module"
