-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Module
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The CoreFn module representation
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Module where

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.Types
import qualified Language.PureScript.AST as A

data Module = Module
  { moduleName :: ModuleName
  , moduleDecls :: [Bind]
  , moduleForeign :: [ForeignDecl]
  , moduleExports :: [A.DeclarationRef]
  }

type ForeignDecl = (Ident, Maybe JS, Type)
