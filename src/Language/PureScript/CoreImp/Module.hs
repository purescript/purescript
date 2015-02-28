-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp.Module
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The CoreImp module representation
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp.Module where

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments
import Language.PureScript.Names
import Language.PureScript.Types

data Module a = Module
  { moduleComments :: [Comment]
  , moduleName :: ModuleName
  , moduleImports :: [ModuleName]
  , moduleExports :: [Ident]
  , moduleForeign :: [ForeignDecl]
  , moduleStatements :: [Statement a]
  } deriving (Show)

type ForeignDecl = (Ident, Maybe JS, Type)
