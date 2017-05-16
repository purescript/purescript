module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , CompletionOptions(..)
       , defaultCompletionOptions
       , applyCompletionOptions
       ) where

import           Protolude

import           Data.Aeson
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import qualified Language.PureScript as P

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
  }

defaultCompletionOptions :: CompletionOptions
defaultCompletionOptions = CompletionOptions { coMaxResults = Nothing }

applyCompletionOptions :: CompletionOptions -> [a] -> [a]
applyCompletionOptions co =
  maybe identity take (coMaxResults co)

instance FromJSON CompletionOptions where
  parseJSON = withObject "CompletionOptions" $ \o -> do
    maxResults <- o .:? "maxResults"
    pure (CompletionOptions { coMaxResults = maxResults })

