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
       , moduleFilter
       , prefixFilter
       , equalityFilter
       , applyFilters
       , namespaceFilter
       ) where

import           Protolude                     hiding (isPrefixOf)

import           Data.Aeson
import           Data.Text                     (isPrefixOf)
import           Data.List.NonEmpty            (NonEmpty)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript           as P

newtype Filter = Filter (Endo [Module])
  deriving (Monoid)

type Module = (P.ModuleName, [IdeDeclarationAnn])

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
prefixFilter t =
  mkFilter $ identFilter prefix t
  where
    prefix :: IdeDeclaration -> Text -> Bool
    prefix ed search = search `isPrefixOf` identifierFromIdeDeclaration ed

-- | Only keeps Identifiers that are equal to the search string
equalityFilter :: Text -> Filter
equalityFilter =
  mkFilter . identFilter equality
  where
    equality :: IdeDeclaration -> Text -> Bool
    equality ed search = identifierFromIdeDeclaration ed == search

-- | Keeps Identifiers only, that are equal to a given namespace
namespaceFilter :: NonEmpty IdeNamespace -> Filter
namespaceFilter =
  mkFilter . namespaceFilter'

namespaceFilter' :: NonEmpty IdeNamespace -> [Module] -> [Module]
namespaceFilter' nss modules =
  filter (not . null . snd) $
    foldl (\modules' ns -> modules' `mappend` fmap (filterModuleDecls ns) modules) [] nss
  where
    filterModuleDecls :: IdeNamespace -> Module -> Module
    filterModuleDecls ns' (moduleIdent, decls) =
      (moduleIdent, filter (filterNsByDecl ns') decls)
    filterNsByDecl :: IdeNamespace -> IdeDeclarationAnn -> Bool
    filterNsByDecl IdeNSType decl = isIdeNSType decl
    filterNsByDecl IdeNSValue decl = isIdeNSValue decl
    filterNsByDecl IdeNSKind decl = isIdeNSKind decl
    filterNsByDecl _ _ = False

identFilter :: (IdeDeclaration -> Text -> Bool) -> Text -> [Module] -> [Module]
identFilter predicate search =
  filter (not . null . snd) . fmap filterModuleDecls
  where
    filterModuleDecls :: Module -> Module
    filterModuleDecls (moduleIdent, decls) =
        (moduleIdent, filter (flip predicate search . discardAnn) decls)

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
        modules <- fmap P.moduleNameFromString <$> params .: "modules"
        return $ moduleFilter modules
      "namespace" -> do
        params <- o .: "params"
        nss <- fmap ideNamespaceFromString <$> params .: "namespaces"
        return $ namespaceFilter nss
      _ -> mzero
