-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript
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

module Language.PureScript (module P, compile) where

import Language.PureScript.Values as P
import Language.PureScript.Types as P
import Language.PureScript.Kinds as P
import Language.PureScript.Declarations as P
import Language.PureScript.Names as P
import Language.PureScript.Parser as P
import Language.PureScript.CodeGen as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Pretty as P
import Language.PureScript.Sugar as P
import Language.PureScript.Options as P

import Data.List (intercalate)
import Control.Monad (when, forM)
import Control.Applicative ((<$>))
import qualified Data.Map as M

compile :: Options -> [Module] -> Either String (String, String, Environment)
compile opts ms = do
  desugared <- desugar ms
  (elaborated, env) <- runCheck $ forM desugared $ \(Module moduleName decls) -> Module moduleName <$> typeCheckAll (ModuleName moduleName) decls
  let regrouped = createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let js = concatMap (flip (moduleToJs opts) env) $ regrouped
  let exts = intercalate "\n" . map (flip moduleToPs env) $ regrouped
  js' <- case () of
              _ | optionsRunMain opts -> do
                    when ((ModuleName (ProperName "Main"), Ident "main") `M.notMember` (names env)) $
                      Left "Main.main is undefined"
                    return $ js ++ [JSApp (JSAccessor "main" (JSVar (Ident "Main"))) []]
                | otherwise -> return js
  return (prettyPrintJS js', exts, env)
