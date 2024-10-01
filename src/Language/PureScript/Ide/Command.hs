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

import Protolude

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON(..), withObject, (.!=), (.:), (.:?))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript qualified as P
import Language.PureScript.Ide.CaseSplit (WildcardAnnotations, explicitAnnotations, noAnnotations)
import Language.PureScript.Ide.Completion (CompletionOptions, defaultCompletionOptions)
import Language.PureScript.Ide.Filter (Filter)
import Language.PureScript.Ide.Matcher (Matcher, Matcher')
import Language.PureScript.Ide.Types (IdeDeclarationAnn, IdeNamespace)

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
      , completeMatcher       :: Maybe Matcher'
      , completeCurrentModule :: Maybe P.ModuleName
      , completeOptions       :: CompletionOptions
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
    | FindUsages
      { usagesModule :: P.ModuleName
      , usagesIdentifier :: Text
      , usagesNamespace :: IdeNamespace
      }
      -- Import InputFile OutputFile
    | Import FilePath (Maybe FilePath) [Filter] ImportCommand
    | List { listType :: ListType }
    | Rebuild FilePath (Maybe FilePath) (Set P.CodegenTarget)
    | RebuildSync FilePath (Maybe FilePath) (Set P.CodegenTarget)
    | Cwd
    | Reset
    | Quit

commandName :: Command -> Text
commandName c = case c of
  Load{} -> "Load"
  LoadSync{} -> "LoadSync"
  Type{} -> "Type"
  Complete{} -> "Complete"
  CaseSplit{} -> "CaseSplit"
  AddClause{} -> "AddClause"
  FindUsages{} -> "FindUsages"
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

      s -> fail ("Unknown import command: " <> show s)

data ListType = LoadedModules | Imports FilePath | AvailableModules

instance FromJSON ListType where
  parseJSON = withObject "ListType" $ \o -> do
    (listType' :: Text) <- o .: "type"
    case listType' of
      "import" -> Imports <$> o .: "file"
      "loadedModules" -> pure LoadedModules
      "availableModules" -> pure AvailableModules
      s -> fail ("Unknown list type: " <> show s)

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
          <*> params .:? "matcher" .!= Nothing
          <*> (fmap P.moduleNameFromString <$> params .:? "currentModule")
          <*> params .:? "options" .!= defaultCompletionOptions
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
      "usages" -> do
        params <- o .: "params"
        FindUsages
          <$> map P.moduleNameFromString (params .: "module")
          <*> params .: "identifier"
          <*> params .: "namespace"
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
          <*> (parseCodegenTargets =<< params .:? "codegen" .!= [ "js" ])
      c -> fail ("Unknown command: " <> show c)
    where
      parseCodegenTargets ts =
        case traverse (\t -> Map.lookup t P.codegenTargets) ts of
          Nothing ->
            fail ("Failed to parse codegen targets: " <> show ts)
          Just ts' ->
            pure (Set.fromList ts')

      mkAnnotations True = explicitAnnotations
      mkAnnotations False = noAnnotations
