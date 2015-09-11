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

import Language.PureScript.Comments
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.Types

data Module a = Module
  { moduleComments :: [Comment]
  , moduleName :: ModuleName
  , moduleImports :: [ModuleName]
  , moduleExports :: [Ident]
  , moduleForeign :: [ForeignDecl]
  , moduleDecls :: [Bind a]
  } deriving (Show, Read)

type ForeignDecl = (Ident, Type)
