module Language.PureScript.CoreFn.Module where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names

-- |
-- The CoreFn module representation
--
-- The json CoreFn representation does not contain type information.  When
-- parsing it one gets back `ModuleT () Ann` rathern than `ModuleT Type Ann`,
-- which is enough for `moduleToJs`.
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  } deriving (Show)
