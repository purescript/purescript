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

moduleToPs :: Module -> Environment -> String
moduleToPs (Module pname@(ProperName moduleName) decls) env =
  "module " ++ moduleName ++ " where\n" ++
  (intercalate "\n" . map ("  " ++) . concatMap (declToPs (ModuleName pname) env) $ decls)

declToPs :: ModuleName -> Environment -> Declaration -> [String]
declToPs path env (ValueDeclaration name _ _ _) = maybeToList $ do
  (ty, _) <- M.lookup (path, name) $ names env
  return $ "foreign import " ++ show name ++ " :: " ++ prettyPrintType ty
declToPs path env (BindingGroupDeclaration vals) = do
  flip mapMaybe vals $ \(name, _) -> do
    (ty, _) <- M.lookup (path, name) $ names env
    return $ "foreign import " ++ show name ++ " :: " ++ prettyPrintType ty
declToPs path env (DataDeclaration name _ _) = maybeToList $ do
  (kind, _) <- M.lookup (path, name) $ types env
  return $ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
declToPs _ _ (ExternDataDeclaration name kind) =
  return $ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
declToPs _ _ (TypeSynonymDeclaration name args ty) =
  return $ "type " ++ show name ++ " " ++ unwords args ++ " = " ++ prettyPrintType ty
declToPs _ _ _ = []
