-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer.Unused
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Removes unused variables
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.JS.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUnusedArg
  , removeUndefinedApp
  ) where

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Optimizer.Common

import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: JS -> JS
removeCodeAfterReturnStatements = everywhereOnJS (removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go jss | not (any isJSReturn jss) = jss
         | otherwise = let (body, ret : _) = break isJSReturn jss in body ++ [ret]
  isJSReturn (JSReturn _) = True
  isJSReturn _ = False

removeUnusedArg :: JS -> JS
removeUnusedArg = everywhereOnJS convert
  where
  convert (JSFunction name [arg] body) | arg == C.__unused = JSFunction name [] body
  convert js = js

removeUndefinedApp :: JS -> JS
removeUndefinedApp = everywhereOnJS convert
  where
  convert (JSApp fn [JSVar arg]) | arg == C.undefined = JSApp fn []
  convert js = js
