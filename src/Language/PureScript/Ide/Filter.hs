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

module Language.PureScript.Ide.Filter
       ( Filter
       , namespaceFilter
       , moduleFilter
       , prefixFilter
       , equalityFilter
       , applyFilters
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Data.Aeson
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Text                     (isPrefixOf)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript           as P

newtype Filter = Filter (Endo [Module])
  deriving (Monoid)

type Module = (P.ModuleName, [IdeDeclarationAnn])

mkFilter :: ([Module] -> [Module]) -> Filter
mkFilter = Filter . Endo

-- | Only keeps Identifiers in the given Namespaces
namespaceFilter :: NonEmpty IdeNamespace -> Filter
namespaceFilter namespaces =
  mkFilter (filterModuleDecls filterNamespaces)
  where
    filterNamespaces :: IdeDeclaration -> Bool
    filterNamespaces decl = elem (namespaceForDeclaration decl) namespaces

-- | Only keeps the given Modules
moduleFilter :: [P.ModuleName] -> Filter
moduleFilter =
    mkFilter . moduleFilter'

moduleFilter' :: [P.ModuleName] -> [Module] -> [Module]
moduleFilter' moduleIdents = filter (flip elem moduleIdents . fst)

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> Filter
prefixFilter "" = mkFilter identity
prefixFilter t =
  mkFilter $ declarationFilter prefix t
  where
    prefix :: IdeDeclaration -> Text -> Bool
    prefix ed search = search `isPrefixOf` identifierFromIdeDeclaration ed

-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> Filter
equalityFilter =
  mkFilter . declarationFilter equality
  where
    equality :: IdeDeclaration -> Text -> Bool
    equality ed search = identifierFromIdeDeclaration ed == search

declarationFilter :: (IdeDeclaration -> Text -> Bool) -> Text -> [Module] -> [Module]
declarationFilter predicate search =
  filterModuleDecls (flip predicate search)

filterModuleDecls :: (IdeDeclaration -> Bool) -> [Module] -> [Module]
filterModuleDecls predicate =
  filter (not . null . snd) . fmap filterDecls
  where
    filterDecls (moduleIdent, decls) = (moduleIdent, filter (predicate . discardAnn) decls)

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
      "namespace" -> do
        params <- o .: "params"
        namespaces <- params .: "namespaces"
        return $ namespaceFilter namespaces
      _ -> mzero
