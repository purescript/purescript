-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.Unused
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

module Language.PureScript.Optimizer.Unused (
  removeCodeAfterReturnStatements
) where

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Optimizer.Common

removeCodeAfterReturnStatements :: JS -> JS
removeCodeAfterReturnStatements = everywhereOnJS (removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go jss | not (any isJSReturn jss) = jss
         | otherwise = let (body, ret : _) = span (not . isJSReturn) jss in body ++ [ret]
  isJSReturn (JSReturn _) = True
  isJSReturn _ = False
