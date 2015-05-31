-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Core.Module
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The Core module representation
--
-----------------------------------------------------------------------------

module Language.PureScript.Core.Module where

import Language.PureScript.Comments
import Language.PureScript.Names
import Language.PureScript.Types

data Module a f = Module
  { moduleComments :: [Comment]
  , moduleName :: ModuleName
  , moduleImports :: [ModuleName]
  , moduleExports :: [Ident]
  , moduleForeign :: [ForeignDecl]
  , moduleDecls :: [a]
  } deriving (Show)

type ForeignDecl = (Ident, Type)
