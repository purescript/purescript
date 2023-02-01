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

module Language.PureScript.Ide.Filter
       ( Filter
       , moduleFilter
       , namespaceFilter
       , exactFilter
       , prefixFilter
       , declarationTypeFilter
       , dependencyFilter
       , applyFilters
       ) where

import           Protolude                     hiding (isPrefixOf, Prefix)

import           Control.Monad.Fail (fail)
import           Data.Aeson
import           Data.Text (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Language.PureScript.Ide.Filter.Declaration (DeclarationType)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Imports
import           Language.PureScript.Ide.Util

import qualified Language.PureScript           as P
import qualified Data.Text as T

import Language.PureScript.Ide.Filter.Imports 

newtype Filter = Filter (Either (Set P.ModuleName) DeclarationFilter)
  deriving Show

unFilter :: Filter -> Either (Set P.ModuleName) DeclarationFilter
unFilter (Filter f) = f

data DeclarationFilter
  = Prefix Text
  | Exact Text
  | Namespace (Set IdeNamespace)
  | DeclType (Set DeclarationType)
  | Dependencies { qualifier :: Maybe P.ModuleName, currentModuleName :: P.ModuleName, dependencyImports :: [Import] }
  deriving Show

-- | Only keeps Declarations in the given modules
moduleFilter :: Set P.ModuleName -> Filter
moduleFilter = Filter . Left

-- | Only keeps Identifiers in the given Namespaces
namespaceFilter :: Set IdeNamespace -> Filter
namespaceFilter nss = Filter (Right (Namespace nss))

-- | Only keeps Identifiers that are equal to the search string
exactFilter :: Text -> Filter
exactFilter t = Filter (Right (Exact t))

-- | Only keeps Identifiers that start with the given prefix
prefixFilter :: Text -> Filter
prefixFilter t = Filter (Right (Prefix t))

-- | Only keeps Identifiers in the given type declarations
declarationTypeFilter :: Set DeclarationType -> Filter
declarationTypeFilter dts = Filter (Right (DeclType dts))

dependencyFilter :: Maybe P.ModuleName -> P.ModuleName -> [Import] -> Filter
dependencyFilter q m f = Filter (Right (Dependencies q m f))

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
  . Map.mapWithKey (\modl decls -> foldr (.) identity (map (applyDeclarationFilter modl) fs) decls)

applyDeclarationFilter
  :: P.ModuleName
  -> DeclarationFilter
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
applyDeclarationFilter modl f = case f of
  Prefix prefix -> prefixFilter' prefix
  Exact t -> exactFilter' t
  Namespace namespaces -> namespaceFilter' namespaces
  DeclType dts -> declarationTypeFilter' dts
  Dependencies qual currentModuleName imps -> dependencyFilter' modl qual currentModuleName imps

namespaceFilter' :: Set IdeNamespace -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
namespaceFilter' namespaces =
  filter (\decl -> namespaceForDeclaration (discardAnn decl) `elem` namespaces)

exactFilter' :: Text -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
exactFilter' search =
  filter (\decl -> identifierFromIdeDeclaration (discardAnn decl) == search)

prefixFilter' :: Text -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
prefixFilter' prefix =
  filter (\decl -> prefix `isPrefixOf` identifierFromIdeDeclaration (discardAnn decl))

declarationTypeFilter' :: Set DeclarationType -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
declarationTypeFilter' declTypes =
  filter (\decl -> declarationType (discardAnn decl) `Set.member` declTypes)

dependencyFilter' :: P.ModuleName -> Maybe P.ModuleName -> P.ModuleName -> [Import] -> [IdeDeclarationAnn] -> [IdeDeclarationAnn]
dependencyFilter' modl qual currentModuleName imports =
  if modl == currentModuleName && isNothing qual then
    identity
  else
    filter (\decl -> any (matchImport qual modl decl) imports)

instance FromJSON Filter where
  parseJSON = withObject "filter" $ \o -> do
    (filter' :: Text) <- o .: "filter"
    case filter' of
      "modules" -> do
        params <- o .: "params"
        modules <- map P.moduleNameFromString <$> params .: "modules"
        pure (moduleFilter (Set.fromList modules))
      "exact" -> do
        params <- o .: "params"
        search <- params .: "search"
        pure (exactFilter search)
      "prefix" -> do
        params <- o .: "params"
        search <- params .: "search"
        pure (prefixFilter search)
      "namespace" -> do
        params <- o .: "params"
        namespaces <- params .: "namespaces"
        pure (namespaceFilter (Set.fromList namespaces))
      "declarations" -> do
        declarations <- o .: "params"
        pure (declarationTypeFilter (Set.fromList declarations))
      "dependencies" -> do
        params <- o .: "params"
        moduleText <- params .: "moduleText"
        qualifier <- fmap P.moduleNameFromString <$> params .:? "qualifier"
        case sliceImportSection (T.lines moduleText) of
          Left err -> fail ("Couldn't parse module imports: " <> T.unpack err)
          Right (currentModuleName, _, imports, _ ) -> pure (dependencyFilter qualifier currentModuleName imports)
      s -> fail ("Unknown filter: " <> show s)
