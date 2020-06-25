module Language.PureScript.CoreFn.Module where

import Prelude.Compat

import Data.Map.Strict (Map)

import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names

-- |
-- The CoreFn module representation
--
-- The json CoreFn representation does not contain type information.  When
-- parsing it one gets back `ModuleT () Ann` rather than `ModuleT Type Ann`,
-- which is enough for `moduleToJs`.
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  } deriving (Show)
