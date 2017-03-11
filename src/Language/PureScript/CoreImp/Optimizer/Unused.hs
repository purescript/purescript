{-# LANGUAGE GADTs #-}

-- | Removes unused variables
module Language.PureScript.CoreImp.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUnusedArg
  , removeUndefinedApp
  ) where

import Prelude.Compat

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: AST ty ann -> AST ty ann
removeCodeAfterReturnStatements = everywhere (removeFromBlock go) where
  go :: [AST ty ann] -> [AST ty ann]
  go jss | not (any isReturn jss) = jss
         | otherwise = let (body, ret : _) = break isReturn jss in body ++ [ret]
  isReturn (Return _ _) = True
  isReturn (ReturnNoResult _) = True
  isReturn _ = False

removeUnusedArg :: AST ty ann -> AST ty ann
removeUnusedArg = everywhere convert where
  convert :: AST ty ann -> AST ty ann
  convert (Function ss name [arg] body) | arg == C.__unused = Function ss name [] body
  convert js = js

removeUndefinedApp :: AST ty ann -> AST ty ann
removeUndefinedApp = everywhere convert where
  convert :: AST ty ann -> AST ty ann
  convert (App ss fn [Var _ arg]) | arg == C.undefined = App ss fn []
  convert js = js
