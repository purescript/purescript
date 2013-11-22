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

import Data.List (intercalate)
import Data.Maybe (mapMaybe)

compile :: [Declaration] -> Either String (String, String, Environment)
compile decls = do
  bracketted <- rebracket decls
  (_, env) <- runCheck (typeCheckAll bracketted)
  let js = prettyPrintJS . map optimize . concat . mapMaybe (\decl -> declToJs Nothing global decl env) $ bracketted
  let exts = intercalate "\n" . mapMaybe (externToPs 0 global env) $ bracketted
  return (js, exts, env)
