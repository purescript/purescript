-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Filter
-- Description : Filters for psc-ide commands
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Filters for psc-ide commands
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.PureScript.Ide.Filter
       ( Filter
       , moduleFilter
       , prefixFilter
       , equalityFilter
       , dependencyFilter
       , runFilter
       , applyFilters
       ) where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import           Data.Maybe                    (listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Text                     (Text, isPrefixOf)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

newtype Filter = Filter (Endo [Module]) deriving(Monoid)

mkFilter :: ([Module] -> [Module]) -> Filter
mkFilter = Filter . Endo

-- | Only keeps the given Modules
moduleFilter :: [ModuleIdent] -> Filter
moduleFilter =
    mkFilter . moduleFilter'

moduleFilter' :: [ModuleIdent] -> [Module] -> [Module]
moduleFilter' moduleIdents = filter (flip elem moduleIdents . fst)

-- | Only keeps the given Modules and all of their dependencies
dependencyFilter :: [ModuleIdent] -> Filter
dependencyFilter = mkFilter . dependencyFilter'

dependencyFilter' :: [ModuleIdent] -> [Module] -> [Module]
dependencyFilter' moduleIdents mods =
  moduleFilter' (concatMap (getDepForModule mods) moduleIdents) mods
  where
    getDepForModule :: [Module] -> ModuleIdent -> [ModuleIdent]
    getDepForModule ms moduleIdent =
      moduleIdent : maybe [] extractDeps (findModule moduleIdent ms)

    findModule :: ModuleIdent -> [Module] -> Maybe Module
    findModule i ms = listToMaybe $ filter go ms
      where go (mn, _) = i == mn

    extractDeps :: Module -> [ModuleIdent]
    extractDeps = mapMaybe extractDep . snd
      where extractDep (Dependency n _ _) = Just n
            extractDep _ = Nothing

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> Filter
prefixFilter "" = mkFilter id
prefixFilter t = mkFilter $ identFilter prefix t
  where
    prefix :: ExternDecl -> Text -> Bool
    prefix Export{} _ = False
    prefix Dependency{} _ = False
    prefix ed search = search `isPrefixOf` identifierFromExternDecl ed


-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> Filter
equalityFilter = mkFilter . identFilter equality
  where
    equality :: ExternDecl -> Text -> Bool
    equality ed search = identifierFromExternDecl ed == search

identFilter :: (ExternDecl -> Text -> Bool ) -> Text -> [Module] -> [Module]
identFilter predicate search =
    filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent,decls) =
        (moduleIdent, filter (`predicate` search) decls)

runFilter :: Filter -> [Module] -> [Module]
runFilter (Filter f)= appEndo f

applyFilters :: [Filter] -> [Module] -> [Module]
applyFilters = runFilter . fold

instance FromJSON Filter where
  parseJSON = withObject "filter" $ \o -> do
    (filter' :: String) <- o .: "filter"
    case filter' of
      "exact" -> do
        params <- o .: "params"
        search <- params .: "search"
        return $ equalityFilter search
      "prefix" -> do
        params <- o.: "params"
        search <- params .: "search"
        return $ prefixFilter search
      "modules" -> do
        params <- o .: "params"
        modules <- params .: "modules"
        return $ moduleFilter modules
      "dependencies" -> do
        params <- o .: "params"
        deps <- params .: "modules"
        return $ dependencyFilter deps
      _ -> mzero
