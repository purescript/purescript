-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp.Optimizer.Unused
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused code
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp.Optimizer.Unused where

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import Language.PureScript.CoreImp.Traversals
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: Decl a -> Decl a
removeCodeAfterReturnStatements = removeFromBlock (go isReturn) (go isLoopReturn)
  where
  go test ss | not (any test ss) = ss
             | otherwise = let (body, ret : _) = break test ss in body ++ [ret]
  isReturn (Return{}) = True
  isReturn _ = False
  isLoopReturn (Statement s) = isReturn s
  isLoopReturn _ = False

removeUnusedArg :: Decl a -> Decl a
removeUnusedArg = go
  where
  (go, _, _, _) = everywhere id convert id id
  convert (AnonFunction ann [arg] body) | runIdent arg == C.__unused = AnonFunction ann [] body
  convert other = other

removeUndefinedApp :: Decl a -> Decl a
removeUndefinedApp = go
  where
  (go, _, _, _) = everywhere id convert id id
  convert (App ann fn [Var _ arg]) | arg == undef = App ann fn []
  convert other = other
  undef = Qualified (Just (ModuleName [ProperName C.prim])) (Ident C.undefined)
