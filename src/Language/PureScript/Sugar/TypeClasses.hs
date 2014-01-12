-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeClasses
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.TypeClasses (
  desugarTypeClasses
) where

import Language.PureScript.Declarations

desugarTypeClasses :: [Module] -> [Module]
desugarTypeClasses = map desugarModule

desugarModule :: Module -> Module
desugarModule (Module name decls) = Module name $ concatMap desugarDecl decls

desugarDecl :: Declaration -> [Declaration]
desugarDecl other = [other]
