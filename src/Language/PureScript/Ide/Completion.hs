{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       ) where

import           Protolude

import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions
  :: [Filter]
  -> Matcher IdeDeclaration
  -> [Module]
  -> [Match IdeDeclaration]
getCompletions filters matcher modules =
  runMatcher matcher (completionsFromModules discardAnn (applyFilters filters modules))

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  completionsFromModules identity (applyFilters (equalityFilter search : filters) modules)

completionsFromModules :: (IdeDeclarationAnn -> a) -> [Module] -> [Match a]
completionsFromModules f = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, f x)) decls
