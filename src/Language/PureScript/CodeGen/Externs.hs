-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Externs
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code for \"externs\" files, i.e. files containing only foreign import declarations.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Externs (
    moduleToPs
) where

import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Map as M
import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty
import Language.PureScript.Names
import Data.List (intercalate)

-- |
-- Generate foreign imports for all declarations in a module
--
moduleToPs :: Module -> Environment -> String
moduleToPs (Module mn decls) env =
  "module " ++ runModuleName mn ++ " where\n" ++
  (intercalate "\n" . map ("  " ++) . concatMap (declToPs mn env) $ decls)

declToPs :: ModuleName -> Environment -> Declaration -> [String]
declToPs path env (ValueDeclaration name _ _ _) = maybeToList $ do
  (ty, _) <- M.lookup (path, name) $ names env
  return $ "foreign import " ++ show name ++ " :: " ++ prettyPrintType ty
declToPs path env (BindingGroupDeclaration vals) =
  flip mapMaybe vals $ \(name, _) -> do
    (ty, _) <- M.lookup (path, name) $ names env
    return $ "foreign import " ++ show name ++ " :: " ++ prettyPrintType ty
declToPs path env (DataDeclaration name _ _) = maybeToList $ do
  kind <- M.lookup (Qualified (Just path) name) $ types env
  return $ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
declToPs _ _ (ExternDataDeclaration name kind) =
  return $ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
declToPs _ _ (TypeSynonymDeclaration name args ty) =
  return $ "type " ++ show name ++ " " ++ unwords args ++ " = " ++ prettyPrintType ty
declToPs _ _ _ = []
