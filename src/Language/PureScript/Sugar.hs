-- |
-- Desugaring passes
--
module Language.PureScript.Sugar (desugar, module S) where

import Prelude

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)

import Language.PureScript.AST (Module)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Linter.Imports (UsedImports)
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
import Language.PureScript.Sugar.Accessor as S

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
--  * Introduce newtypes for type class dictionaries and value declarations for instances
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
    >>> fmap (desugarAccessorModule externs)
    >=> desugarDoModule
    >=> desugarAdoModule
    >=> desugarLetPatternModule
    >>> desugarCasesModule
    >=> desugarTypeDeclarationsModule
    >=> desugarImports
    >=> rebracket externs
    >=> checkFixityExports
    >=> deriveInstances
    >=> desugarTypeClasses externs
    >=> createBindingGroupsModule
