{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.Completion
       (getCompletions, getExactMatches)
       where

import           Prelude                         ()
import           Prelude.Compat

import           Data.Maybe                      (mapMaybe)
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions :: [Filter] -> Matcher -> [Module] -> [Completion]
getCompletions filters matcher modules =
    runMatcher matcher $ completionsFromModules (applyFilters filters modules)

getExactMatches :: DeclIdent -> [Filter] -> [Module] -> [Completion]
getExactMatches search filters modules =
    completionsFromModules $
    applyFilters (equalityFilter search : filters) modules

completionsFromModules :: [Module] -> [Completion]
completionsFromModules = foldMap completionFromModule
    where
        completionFromModule :: Module -> [Completion]
        completionFromModule (moduleIdent, decls) = mapMaybe (completionFromDecl moduleIdent) decls

completionFromDecl :: ModuleIdent -> ExternDecl -> Maybe Completion
completionFromDecl mi (FunctionDecl name type') = Just (Completion (mi, name, type'))
completionFromDecl mi (DataDecl name kind)      = Just (Completion (mi, name, kind))
completionFromDecl _ (ModuleDecl name _)        = Just (Completion ("module", name, "module"))
completionFromDecl _ _                          = Nothing
