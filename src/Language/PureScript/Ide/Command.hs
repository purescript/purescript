-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Command
-- Description : Datatypes for the commands psc-ide accepts
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Datatypes for the commands psc-ide accepts
-----------------------------------------------------------------------------

module Language.PureScript.Ide.Command where

import           Protolude

import           Data.Aeson
import qualified Language.PureScript               as P
import           Language.PureScript.Ide.CaseSplit
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

data Command
    = Load [P.ModuleName]
    | LoadSync [P.ModuleName] -- used in tests
    | Type
      { typeSearch        :: Text
      , typeFilters       :: [Filter]
      , typeCurrentModule :: Maybe P.ModuleName
      }
    | Complete
      { completeFilters       :: [Filter]
      , completeMatcher       :: Matcher IdeDeclarationAnn
      , completeCurrentModule :: Maybe P.ModuleName
      , completeOptions       :: CompletionOptions
      }
    | Pursuit
      { pursuitQuery      :: PursuitQuery
      , pursuitSearchType :: PursuitSearchType
      }
    | CaseSplit
      { caseSplitLine        :: Text
      , caseSplitBegin       :: Int
      , caseSplitEnd         :: Int
      , caseSplitAnnotations :: WildcardAnnotations
      , caseSplitType        :: Text
      }
    | AddClause
      { addClauseLine        :: Text
      , addClauseAnnotations :: WildcardAnnotations
      }
      -- Import InputFile OutputFile
    | Import FilePath (Maybe FilePath) [Filter] ImportCommand
    | List { listType :: ListType }
    | Rebuild FilePath (Maybe FilePath)
    | RebuildSync FilePath (Maybe FilePath)
    | Cwd
    | Reset
    | Quit

commandName :: Command -> Text
commandName c = case c of
  Load{} -> "Load"
  LoadSync{} -> "LoadSync"
  Type{} -> "Type"
  Complete{} -> "Complete"
  Pursuit{} -> "Pursuit"
  CaseSplit{} -> "CaseSplit"
  AddClause{} -> "AddClause"
  Import{} -> "Import"
  List{} -> "List"
  Rebuild{} -> "Rebuild"
  RebuildSync{} -> "RebuildSync"
  Cwd{} -> "Cwd"
  Reset{} -> "Reset"
  Quit{} -> "Quit"

data ImportCommand
  = AddImplicitImport P.ModuleName
  | AddQualifiedImport P.ModuleName P.ModuleName
  | AddImportForIdentifier Text (Maybe P.ModuleName)
  deriving (Show, Eq)

instance FromJSON ImportCommand where
  parseJSON = withObject "ImportCommand" $ \o -> do
    (command :: Text) <- o .: "importCommand"
    case command of
      "addImplicitImport" ->
        AddImplicitImport <$> (P.moduleNameFromString <$> o .: "module")
      "addQualifiedImport" ->
        AddQualifiedImport
          <$> (P.moduleNameFromString <$> o .: "module")
          <*> (P.moduleNameFromString <$> o .: "qualifier")
      "addImport" ->
        AddImportForIdentifier
          <$> (o .: "identifier")
          <*> (fmap P.moduleNameFromString <$> o .:? "qualifier")

      _ -> mzero

data ListType = LoadedModules | Imports FilePath | AvailableModules

instance FromJSON ListType where
  parseJSON = withObject "ListType" $ \o -> do
    (listType' :: Text) <- o .: "type"
    case listType' of
      "import" -> Imports <$> o .: "file"
      "loadedModules" -> pure LoadedModules
      "availableModules" -> pure AvailableModules
      _ -> mzero

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    (command :: Text) <- o .: "command"
    case command of
      "list" -> List <$> o .:? "params" .!= LoadedModules
      "cwd"  -> pure Cwd
      "quit" -> pure Quit
      "reset" -> pure Reset
      "load" -> do
        params' <- o .:? "params"
        case params' of
          Nothing -> pure (Load [])
          Just params ->
            Load <$> (map P.moduleNameFromString <$> params .:? "modules" .!= [])
      "type" -> do
        params <- o .: "params"
        Type
          <$> params .: "search"
          <*> params .:? "filters" .!= []
          <*> (fmap P.moduleNameFromString <$> params .:? "currentModule")
      "complete" -> do
        params <- o .: "params"
        Complete
          <$> params .:? "filters" .!= []
          <*> params .:? "matcher" .!= mempty
          <*> (fmap P.moduleNameFromString <$> params .:? "currentModule")
          <*> params .:? "options" .!= defaultCompletionOptions
      "pursuit" -> do
        params <- o .: "params"
        Pursuit
          <$> params .: "query"
          <*> params .: "type"
      "caseSplit" -> do
        params <- o .: "params"
        CaseSplit
          <$> params .: "line"
          <*> params .: "begin"
          <*> params .: "end"
          <*> (mkAnnotations <$> params .: "annotations")
          <*> params .: "type"
      "addClause" -> do
        params <- o .: "params"
        AddClause
          <$> params .: "line"
          <*> (mkAnnotations <$> params .: "annotations")
      "import" -> do
        params <- o .: "params"
        Import
          <$> params .: "file"
          <*> params .:? "outfile"
          <*> params .:? "filters" .!= []
          <*> params .: "importCommand"
      "rebuild" -> do
        params <- o .: "params"
        Rebuild
          <$> params .: "file"
          <*> params .:? "actualFile"
      _ -> mzero
    where
      mkAnnotations True = explicitAnnotations
      mkAnnotations False = noAnnotations
