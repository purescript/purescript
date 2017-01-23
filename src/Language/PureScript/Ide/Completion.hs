module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       ) where

import           Protolude

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
  -> [Module]
  -> [Match IdeDeclarationAnn]
getCompletions filters matcher modules =
  runMatcher matcher (completionsFromModules (applyFilters filters modules))

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  completionsFromModules (applyFilters (equalityFilter search : filters) modules)

completionsFromModules :: [Module] -> [Match IdeDeclarationAnn]
completionsFromModules = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, x)) decls
