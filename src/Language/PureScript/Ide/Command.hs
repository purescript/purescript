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
import           Language.PureScript.Ide.CaseSplit
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

data Command
    = Load { loadModules      :: [ModuleIdent]
           , loadDependencies :: [ModuleIdent]}
    | Type { typeSearch  :: DeclIdent
           , typeFilters :: [Filter]}
    | Complete { completeFilters :: [Filter]
               , completeMatcher :: Matcher}
    | Pursuit { pursuitQuery      :: PursuitQuery
              , pursuitSearchType :: PursuitSearchType}
    | List {listType :: ListType}
    | CaseSplit {
      caseSplitLine          :: Text
      , caseSplitBegin       :: Int
      , caseSplitEnd         :: Int
      , caseSplitAnnotations :: WildcardAnnotations
      , caseSplitType        :: Type}
    | AddClause {
      addClauseLine          :: Text
      , addClauseAnnotations :: WildcardAnnotations}
    | Cwd
    | Quit

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
      "load" -> do
        params <- o .: "params"
        mods <- params .:? "modules"
        deps <- params .:? "dependencies"
        return $ Load (fromMaybe [] mods) (fromMaybe [] deps)
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
      _ -> mzero

