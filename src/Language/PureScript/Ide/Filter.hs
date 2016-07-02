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
       , applyFilters
       ) where

import           Protolude hiding (isPrefixOf)

import           Data.Aeson
import           Data.Text                     (isPrefixOf)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript as P

newtype Filter = Filter (Endo [Module]) deriving(Monoid)

mkFilter :: ([Module] -> [Module]) -> Filter
mkFilter = Filter . Endo

-- | Only keeps the given Modules
moduleFilter :: [P.ModuleName] -> Filter
moduleFilter =
    mkFilter . moduleFilter'

moduleFilter' :: [P.ModuleName] -> [Module] -> [Module]
moduleFilter' moduleIdents = filter (flip elem moduleIdents . fst)

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> Filter
prefixFilter "" = mkFilter identity
prefixFilter t = mkFilter $ identFilter prefix t
  where
    prefix :: IdeDeclaration -> Text -> Bool
    prefix ed search = search `isPrefixOf` identifierFromIdeDeclaration ed

-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> Filter
equalityFilter = mkFilter . identFilter equality
  where
    equality :: IdeDeclaration -> Text -> Bool
    equality ed search = identifierFromIdeDeclaration ed == search

identFilter :: (IdeDeclaration -> Text -> Bool) -> Text -> [Module] -> [Module]
identFilter predicate search =
    filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent, decls) =
        (moduleIdent, filter (flip predicate search . getDeclaration) decls)
    getDeclaration (IdeDeclarationAnn _ d) = d

runFilter :: Filter -> [Module] -> [Module]
runFilter (Filter f) = appEndo f

applyFilters :: [Filter] -> [Module] -> [Module]
applyFilters = runFilter . fold

instance FromJSON Filter where
  parseJSON = withObject "filter" $ \o -> do
    (filter' :: Text) <- o .: "filter"
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
        modules <- map P.moduleNameFromString <$> params .: "modules"
        return $ moduleFilter modules
      _ -> mzero
