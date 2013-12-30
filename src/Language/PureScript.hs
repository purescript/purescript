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
import Language.PureScript.Optimize as P
import Language.PureScript.Operators as P
import Language.PureScript.CaseDeclarations as P
import Language.PureScript.TypeDeclarations as P
import Language.PureScript.BindingGroups as P

import Data.List (intercalate)
import Control.Monad (forM_, (>=>))

compile :: [Module] -> Either String (String, String, Environment)
compile ms = do
  bracketted <- rebracket ms
  desugared <- desugarCasesModule >=> desugarTypeDeclarationsModule >=> (return . createBindingGroupsModule) $ bracketted
  (_, env) <- runCheck $ forM_ desugared $ \(Module moduleName decls) -> typeCheckAll (ModuleName moduleName) decls
  let js = prettyPrintJS . map optimize . concatMap (flip moduleToJs env) $ desugared
  let exts = intercalate "\n" . map (flip moduleToPs env) $ desugared
  return (js, exts, env)
