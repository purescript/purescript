-- |
-- Removes unused variables
--
module Language.PureScript.CodeGen.JS.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUnusedArg
  , removeUndefinedApp
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Optimizer.Common
import qualified Language.PureScript.Constants as C

removeCodeAfterReturnStatements :: JS -> JS
removeCodeAfterReturnStatements = everywhereOnJS (removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go jss | not (any isJSReturn jss) = jss
         | otherwise = let (body, ret : _) = break isJSReturn jss in body ++ [ret]
  isJSReturn (JSReturn _ _) = True
  isJSReturn _ = False

removeUnusedArg :: JS -> JS
removeUnusedArg = everywhereOnJS convert
  where
  convert (JSFunction ss name [arg] body) | arg == C.__unused = JSFunction ss name [] body
  convert js = js

removeUndefinedApp :: JS -> JS
removeUndefinedApp = everywhereOnJS convert
  where
  convert (JSApp ss fn [JSVar _ arg]) | arg == C.undefined = JSApp ss fn []
  convert js = js
