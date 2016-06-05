{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.Completion
       (getCompletions, getExactMatches)
       where

import           Prelude                         ()
import           Prelude.Compat

import           Data.Text                       (Text)
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions :: [Filter] -> Matcher -> [Module] -> [Match]
getCompletions filters matcher modules =
    runMatcher matcher $ completionsFromModules (applyFilters filters modules)

getExactMatches :: Text -> [Filter] -> [Module] -> [Match]
getExactMatches search filters modules =
    completionsFromModules $
    applyFilters (equalityFilter search : filters) modules

completionsFromModules :: [Module] -> [Match]
completionsFromModules = foldMap completionFromModule
  where
    completionFromModule :: Module -> [Match]
    completionFromModule (moduleName, decls) = map (Match moduleName) decls
