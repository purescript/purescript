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

{-# LANGUAGE OverloadedStrings     #-}

module Language.PureScript.Ide.Command where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Monad
import           Data.Aeson
import           Data.Text                         (Text)
import qualified Language.PureScript               as P
import           Language.PureScript.Ide.CaseSplit
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

data Command
    = Load [P.ModuleName]
    | Type
      { typeSearch        :: Text
      , typeFilters       :: [Filter]
      , typeCurrentModule :: Maybe P.ModuleName
      }
    | Complete
      { completeFilters       :: [Filter]
      , completeMatcher       :: Matcher
      , completeCurrentModule :: Maybe P.ModuleName
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
    | Rebuild FilePath -- ^ Rebuild the specified file using the loaded externs
    | Cwd
    | Reset
    | Quit

data ImportCommand
  = AddImplicitImport P.ModuleName
  | AddImportForIdentifier Text
  deriving (Show, Eq)

instance FromJSON ImportCommand where
  parseJSON = withObject "ImportCommand" $ \o -> do
    (command :: String) <- o .: "importCommand"
    case command of
      "addImplicitImport" ->
        AddImplicitImport <$> (P.moduleNameFromString <$> o .: "module")
      "addImport" ->
        AddImportForIdentifier <$> o .: "identifier"
      _ -> mzero

data ListType = LoadedModules | Imports FilePath | AvailableModules

instance FromJSON ListType where
  parseJSON = withObject "ListType" $ \o -> do
    (listType' :: String) <- o .: "type"
    case listType' of
      "import" -> Imports <$> o .: "file"
      "loadedModules" -> pure LoadedModules
      "availableModules" -> pure AvailableModules
      _ -> mzero

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    (command :: String) <- o .: "command"
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
          <*> params .: "filters"
          <*> (fmap P.moduleNameFromString <$> params .:? "currentModule")
      "complete" -> do
        params <- o .: "params"
        Complete
          <$> params .:? "filters" .!= []
          <*> params .:? "matcher" .!= mempty
          <*> (fmap P.moduleNameFromString <$> params .:? "currentModule")
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
      _ -> mzero
    where
      mkAnnotations True = explicitAnnotations
      mkAnnotations False = noAnnotations
