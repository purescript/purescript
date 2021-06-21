-- |
-- Desugaring passes
--
module Language.PureScript.Sugar (desugar, module S) where

import Prelude

import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Maybe (mapMaybe)

import qualified Data.Map as M

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Linter.Imports
import Language.PureScript.Names
import Language.PureScript.Sugar.BindingGroups as S
import Language.PureScript.Sugar.CaseDeclarations as S
import Language.PureScript.Sugar.DoNotation as S
import Language.PureScript.Sugar.AdoNotation as S
import Language.PureScript.Sugar.LetPattern as S
import Language.PureScript.Sugar.Names as S
import Language.PureScript.Sugar.ObjectWildcards as S
import Language.PureScript.Sugar.Operators as S
import Language.PureScript.Sugar.TypeClasses as S
import Language.PureScript.Sugar.TypeClasses.Deriving as S
import Language.PureScript.Sugar.TypeDeclarations as S
import Language.PureScript.TypeChecker.Synonyms (SynonymMap)

-- |
-- The desugaring pipeline proceeds as follows:
--
--  * Remove signed literals in favour of `negate` applications
--
--  * Desugar object literals with wildcards into lambdas
--
--  * Desugar operator sections
--
--  * Desugar do-notation
--
--  * Desugar ado-notation
--
--  * Desugar top-level case declarations into explicit case expressions
--
--  * Desugar type declarations into value declarations with explicit type annotations
--
--  * Qualify any unqualified names and types
--
--  * Rebracket user-defined binary operators
--
--  * Introduce type synonyms for type class dictionaries
--
--  * Group mutually recursive value and data declarations into binding groups.
--
desugar
  :: MonadSupply m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => MonadState (Env, UsedImports) m
  => [ExternsFile]
  -> Module
  -> m Module
desugar externs =
  desugarSignedLiterals
    >>> desugarObjectConstructors
    >=> desugarDoModule
    >=> desugarAdoModule
    >=> desugarLetPatternModule
    >>> desugarCasesModule
    >=> desugarTypeDeclarationsModule
    >=> desugarImports
    >=> rebracket externs
    >=> checkFixityExports
    >=> (\m ->
      -- We need to collect type synonym information, since synonyms will not be
      -- removed until later, during type checking.
      let syns = findTypeSynonyms externs (getModuleName m) $ getModuleDeclarations m
      -- We cannot prevent ill-kinded expansions of type synonyms without
      -- knowing their kinds but they're not available yet.
          kinds = mempty
       in deriveInstances externs syns kinds m
      >>= desugarTypeClasses externs syns kinds)
    >=> createBindingGroupsModule

findTypeSynonyms :: [ExternsFile] -> ModuleName -> [Declaration] -> SynonymMap
findTypeSynonyms externs mn decls =
    M.fromList $ (externs >>= \ExternsFile{..} -> mapMaybe (fromExternsDecl efModuleName) efDeclarations)
              ++ mapMaybe fromLocalDecl decls
  where
    fromExternsDecl mn' (EDTypeSynonym name args ty) = Just (Qualified (Just mn') name, (args, ty))
    fromExternsDecl _ _ = Nothing

    fromLocalDecl (TypeSynonymDeclaration _ name args ty) =
      Just (Qualified (Just mn) name, (args, ty))
    fromLocalDecl _ = Nothing
