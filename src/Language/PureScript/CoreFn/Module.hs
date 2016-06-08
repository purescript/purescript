module Language.PureScript.CoreFn.Module where

import Prelude.Compat

import Language.PureScript.Comments
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleComments :: [Comment]
  , moduleName :: ModuleName
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleForeign :: [ForeignDecl]
  , moduleDecls :: [Bind a]
  } deriving (Show, Read)

type ForeignDecl = (Ident, Type)
