module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , CompletionOptions(..)
       , defaultCompletionOptions
       , applyCompletionOptions
       ) where

import           Protolude

import           Control.Lens (view)
import           Data.Aeson
import           Data.List (intersect)
import qualified Data.Map as Map
import qualified Language.PureScript as P
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

type Module = (P.ModuleName, [IdeDeclarationAnn])

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions
  :: [Filter]
  -> Matcher IdeDeclarationAnn
  -> CompletionOptions
  -> [Module]
  -> [Match IdeDeclarationAnn]
getCompletions filters matcher options modules =
  modules
  & applyFilters filters
  & completionsFromModules
  & runMatcher matcher
  & applyCompletionOptions options

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  completionsFromModules (applyFilters (equalityFilter search : filters) modules)

completionsFromModules :: [Module] -> [Match IdeDeclarationAnn]
completionsFromModules = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, x)) decls

data CompletionOptions = CompletionOptions
  { coMaxResults :: Maybe Int
  , coGroupReexports :: Maybe [P.ModuleName]
  }

defaultCompletionOptions :: CompletionOptions
defaultCompletionOptions = CompletionOptions { coMaxResults = Nothing, coGroupReexports = Nothing }

applyCompletionOptions :: CompletionOptions -> [Match IdeDeclarationAnn] -> [Match IdeDeclarationAnn]
applyCompletionOptions co decls =
  maybe identity take (coMaxResults co) decls
  & maybe identity groupCompletionReexports (coGroupReexports co)

groupCompletionReexports :: [P.ModuleName] -> [Match IdeDeclarationAnn] -> [Match IdeDeclarationAnn]
groupCompletionReexports preferred initial =
  concatMap choosePreferred (Map.elems (foldr go Map.empty initial))
  where
    go (Match (moduleName, d@(IdeDeclarationAnn (view annExportedFrom -> Just origin) decl))) =
      Map.alter
      (insertReexport moduleName origin d)
      (Namespaced (namespaceForDeclaration decl)
       (P.runModuleName origin <> "." <> identifierFromIdeDeclaration decl))
    go (Match (moduleName, d@(IdeDeclarationAnn _ decl))) =
      Map.alter
      (insertDeclaration moduleName d)
      (Namespaced (namespaceForDeclaration decl)
       (P.runModuleName moduleName <> "." <> identifierFromIdeDeclaration decl))
    insertReexport moduleName origin d old = case old of
      Nothing -> Just (Match (origin, d), [moduleName])
      Just x -> Just (second (moduleName :) x)
    insertDeclaration moduleName d old = case old of
      Nothing -> Just (Match (moduleName, d), [])
      Just x -> Just x

    choosePreferred :: (Match a, [P.ModuleName]) -> [Match a]
    choosePreferred (Match (mn, decl), reexports) =
      case intersect reexports preferred of
        [] -> [Match (mn, decl)]
        prefs -> map (Match . (, decl)) prefs

data Namespaced a = Namespaced IdeNamespace a
  deriving (Show, Eq, Ord)

instance FromJSON CompletionOptions where
  parseJSON = withObject "CompletionOptions" $ \o -> do
    maxResults <- o .:? "maxResults"
    groupReexports <- o .:? "groupReexports"
    pure (CompletionOptions { coMaxResults = maxResults
                            , coGroupReexports = map P.moduleNameFromString <$> groupReexports
                            })
