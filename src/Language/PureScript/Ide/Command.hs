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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.PureScript.Ide.Command where

import           Prelude                           ()
import           Prelude.Compat

import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Text                         (Text)
import           Language.PureScript               (ModuleName,
                                                    moduleNameFromString)
import           Language.PureScript.Ide.CaseSplit
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

data Command
    = Load
      { loadModules      :: [ModuleIdent]
      , loadDependencies :: [ModuleIdent]
      }
    | Type
      { typeSearch  :: DeclIdent
      , typeFilters :: [Filter]
      }
    | Complete
      { completeFilters :: [Filter]
      , completeMatcher :: Matcher
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
    | Quit

data ImportCommand
  = AddImplicitImport ModuleName
  | AddImportForIdentifier DeclIdent
  deriving (Show, Eq)

instance FromJSON ImportCommand where
  parseJSON = withObject "ImportCommand" $ \o -> do
    (command :: String) <- o .: "importCommand"
    case command of
      "addImplicitImport" -> do
        mn <- o .: "module"
        pure (AddImplicitImport (moduleNameFromString mn))
      "addImport" -> do
        ident <- o .: "identifier"
        pure (AddImportForIdentifier ident)
      _ -> mzero

data ListType = LoadedModules | Imports FilePath | AvailableModules

instance FromJSON ListType where
  parseJSON = withObject "ListType" $ \o -> do
    (listType' :: String) <- o .: "type"
    case listType' of
      "import" -> do
        fp <- o .: "file"
        return (Imports fp)
      "loadedModules" -> return LoadedModules
      "availableModules" -> return AvailableModules
      _ -> mzero

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    (command :: String) <- o .: "command"
    case command of
      "list" -> do
        listType' <- o .:? "params"
        return $ List (fromMaybe LoadedModules listType')
      "cwd"  -> return Cwd
      "quit" -> return Quit
      "load" ->
        maybe (pure (Load [] [])) (\params -> do
          mods <- params .:? "modules"
          deps <- params .:? "dependencies"
          pure $ Load (fromMaybe [] mods) (fromMaybe [] deps)) =<< o .:? "params"
      "type" -> do
        params <- o .: "params"
        search <- params .: "search"
        filters <- params .: "filters"
        return $ Type search filters
      "complete" -> do
        params <- o .: "params"
        filters <- params .:? "filters"
        matcher <- params .:? "matcher"
        return $ Complete (fromMaybe [] filters) (fromMaybe mempty matcher)
      "pursuit" -> do
        params <- o .: "params"
        query <- params .: "query"
        queryType <- params .: "type"
        return $ Pursuit query queryType
      "caseSplit" -> do
        params <- o .: "params"
        line <- params .: "line"
        begin <- params .: "begin"
        end <- params .: "end"
        annotations <- params .: "annotations"
        type' <- params .: "type"
        return $ CaseSplit line begin end (if annotations
                                           then explicitAnnotations
                                           else noAnnotations) type'
      "addClause" -> do
        params <- o .: "params"
        line <- params .: "line"
        annotations <- params .: "annotations"
        return $ AddClause line (if annotations
                                 then explicitAnnotations
                                 else noAnnotations)
      "import" -> do
        params <- o .: "params"
        fp <- params .: "file"
        out <- params .:? "outfile"
        filters <- params .:? "filters"
        importCommand <- params .: "importCommand"
        pure $ Import fp out (fromMaybe [] filters) importCommand
      "rebuild" -> do
        params <- o .: "params"
        filePath <- params .: "file"
        return $ Rebuild filePath
      _ -> mzero
