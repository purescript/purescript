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
       ( Filter(..)
       , DeclarationFilter(..)
       , moduleFilter
       , namespaceFilter
       , exactFilter
       , prefixFilter
       , declarationTypeFilter
       , applyFilters
       ) where

import           Protolude                     hiding (isPrefixOf, Prefix)

import           Data.Bifunctor (first)
import           Data.Aeson
import           Data.Text                     (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Language.PureScript.Ide.Filter.Declaration (DeclarationType, declarationType)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import qualified Language.PureScript           as P

newtype Filter = Filter (Either (Set P.ModuleName) DeclarationFilter)
  deriving Show

unFilter :: Filter -> Either (Set P.ModuleName) DeclarationFilter
unFilter (Filter f) = f

data DeclarationFilter
  = Prefix Text
  | Exact Text
  | Namespace (Set IdeNamespace)
  | DeclType (Set DeclarationType)
  deriving Show

moduleFilter :: Set P.ModuleName -> Filter
moduleFilter = Filter . Left

namespaceFilter :: Set IdeNamespace -> Filter
namespaceFilter nss = Filter (Right (Namespace nss))

exactFilter :: Text -> Filter
exactFilter t = Filter (Right (Exact t))

prefixFilter :: Text -> Filter
prefixFilter t = Filter (Right (Prefix t))

declarationTypeFilter :: Set DeclarationType -> Filter
declarationTypeFilter dts = Filter (Right (DeclType dts))

optimizeFilters :: [Filter] -> (Maybe (Set P.ModuleName), [DeclarationFilter])
optimizeFilters = first smashModuleFilters . partitionEithers . map unFilter
  where
    smashModuleFilters [] =
      Nothing
    smashModuleFilters (x:xs) =
      Just (foldr Set.intersection x xs)

applyFilters :: [Filter] -> ModuleMap [IdeDeclarationAnn] -> ModuleMap [IdeDeclarationAnn]
applyFilters fs modules = case optimizeFilters fs of
  (Nothing, declarationFilters) ->
    applyDeclarationFilters declarationFilters modules
  (Just moduleFilter', declarationFilters) ->
    applyDeclarationFilters declarationFilters (Map.restrictKeys modules moduleFilter')

applyDeclarationFilters
  :: [DeclarationFilter]
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
applyDeclarationFilters fs =
  Map.filter (not . null)
  . Map.map (foldr (.) identity (map applyDeclarationFilter fs))

applyDeclarationFilter
  :: DeclarationFilter
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
applyDeclarationFilter f = case f of
  Prefix prefix -> prefixFilter' prefix
  Exact t -> exactFilter' t
  Namespace namespaces -> namespaceFilter' namespaces
  DeclType dts -> declarationTypeFilter' dts

-- | Only keeps Identifiers in the given Namespaces
namespaceFilter' :: Set IdeNamespace -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
namespaceFilter' namespaces =
  filter (\decl -> elem (namespaceForDeclaration (discardAnn decl)) namespaces)

-- | Only keeps Identifiers that are equal to the search string
exactFilter' :: Text -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
exactFilter' search =
  filter (\decl -> identifierFromIdeDeclaration (discardAnn decl) == search)

-- | Only keeps Identifiers that start with the given prefix
prefixFilter' :: Text -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
prefixFilter' prefix =
  filter (\decl -> prefix `isPrefixOf` identifierFromIdeDeclaration (discardAnn decl))

-- | Only keeps Identifiers in the given type declarations
declarationTypeFilter' :: Set DeclarationType -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
declarationTypeFilter' declTypes =
  filter (\decl -> declarationType (discardAnn decl) `Set.member` declTypes)

instance FromJSON Filter where
  parseJSON = withObject "filter" $ \o -> do
    (filter' :: Text) <- o .: "filter"
    Filter <$> case filter' of
      "modules" -> do
        params <- o .: "params"
        modules <- map P.moduleNameFromString <$> params .: "modules"
        pure (Left (Set.fromList modules))
      "exact" -> do
        params <- o .: "params"
        search <- params .: "search"
        pure (Right (Exact search))
      "prefix" -> do
        params <- o.: "params"
        search <- params .: "search"
        pure (Right (Prefix search))
      "namespace" -> do
        params <- o .: "params"
        namespaces <- params .: "namespaces"
        pure (Right (Namespace (Set.fromList namespaces)))
      "declarations" -> do
        declarations <- o.: "params"
        pure (Right (DeclType (Set.fromList declarations)))
      _ -> mzero
