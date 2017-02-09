-- |
-- Desugaring passes
--
module Language.PureScript.Sugar (desugar, module S) where

import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Error.Class (MonadError())
import Control.Monad.Supply.Class
import Control.Monad.Writer.Class (MonadWriter())

import Data.List (map)
import Data.Traversable (traverse)

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Sugar.BindingGroups as S
import Language.PureScript.Sugar.CaseDeclarations as S
import Language.PureScript.Sugar.DoNotation as S
import Language.PureScript.Sugar.LetPattern as S
import Language.PureScript.Sugar.Names as S
import Language.PureScript.Sugar.ObjectWildcards as S
import Language.PureScript.Sugar.Operators as S
import Language.PureScript.Sugar.TypeClasses as S
import Language.PureScript.Sugar.TypeClasses.Deriving as S
import Language.PureScript.Sugar.TypeDeclarations as S

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
  :: (MonadSupply m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [ExternsFile]
  -> [Module]
  -> m [Module]
desugar externs =
  map desugarSignedLiterals
    >>> traverse desugarObjectConstructors
    >=> traverse desugarDoModule
    >=> map desugarLetPatternModule
    >>> traverse desugarCasesModule
    >=> traverse desugarTypeDeclarationsModule
    >=> desugarImports externs
    >=> rebracket externs
    >=> traverse checkFixityExports
    >=> traverse (deriveInstances externs)
    >=> desugarTypeClasses externs
    >=> traverse createBindingGroupsModule

