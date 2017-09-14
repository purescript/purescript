module Language.PureScript.CoreFn.Module where

import Prelude.Compat

import Language.PureScript.Comments
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- The CoreFn module representation
--
data ModuleT t a = Module
  { moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleForeign :: [ForeignDeclT t]
  , moduleDecls :: [Bind a]
  } deriving (Show)

type Module a = ModuleT Type a

type ForeignDeclT t = (Ident, t)

type ForeignDecl = ForeignDeclT Type
