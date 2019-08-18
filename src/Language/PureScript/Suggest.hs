-- |
-- This module implements a simple linting pass on the PureScript AST.
--
module Language.PureScript.Suggest where

import Prelude.Compat

import Data.Maybe (catMaybes, maybeToList)

import qualified Data.List.NonEmpty as NEL

import Language.PureScript.Make.BuildPlan
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import Language.PureScript.Names


decorateSuggestions :: [BuildJobResult] -> [MultipleErrors] -> [MultipleErrors]
decorateSuggestions jobs multipleErrors = decorateMultipleErrors <$> multipleErrors
    where
      compiledExterns :: [ExternsFile]
      compiledExterns = snd <$> (catMaybes $ buildJobSuccess <$> jobs)

      decorateMultipleErrors :: MultipleErrors -> MultipleErrors
      decorateMultipleErrors (MultipleErrors errs) = MultipleErrors $ decorateErrorMessage <$> errs

      decorateErrorMessage :: ErrorMessage -> ErrorMessage
      decorateErrorMessage (ErrorMessage hints err) = ErrorMessage hints (suggestionForSimpleError err)
      decorateErrorMessage others = others

      suggestionForSimpleError ::  SimpleErrorMessage -> SimpleErrorMessage
      suggestionForSimpleError (UnknownName imports qualName@(Qualified Nothing name)) = UnknownName (imports ++ findImportForName name) qualName
      suggestionForSimpleError others = others

      findImportForName :: Name -> [(ModuleName, DeclarationRef)]
      findImportForName name = 
        filter ((==name) . declRefName . snd)
        $ concat
        $ (\extern -> (\decl -> (efModuleName extern, decl)) <$> efExports extern) <$> compiledExterns