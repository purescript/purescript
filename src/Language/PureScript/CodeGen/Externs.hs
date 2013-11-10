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
    externToPs
) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty
import Language.PureScript.Names

externToPs :: Int -> ModulePath -> Environment -> Declaration -> Maybe String
externToPs indent path env (ValueDeclaration name _) = do
  (ty, _) <- M.lookup (path, name) $ names env
  return $ replicate indent ' ' ++ "foreign import " ++ show name ++ " :: " ++ prettyPrintPolyType ty
externToPs indent path env (DataDeclaration name _ _) = do
  (kind, _) <- M.lookup (path, name) $ types env
  return $ replicate indent ' ' ++ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
externToPs indent path env (ExternMemberDeclaration member name ty) =
  return $ replicate indent ' ' ++ "foreign import member " ++ show member ++ " " ++ show name ++ " :: " ++ prettyPrintPolyType ty
externToPs indent path env (ExternDataDeclaration name kind) =
  return $ replicate indent ' ' ++ "foreign import data " ++ show name ++ " :: " ++ prettyPrintKind kind
externToPs indent path env (TypeSynonymDeclaration name args ty) =
  return $ replicate indent ' ' ++ "type " ++ show name ++ " " ++ unwords args ++ " = " ++ prettyPrintType ty
externToPs indent path env (ModuleDeclaration name decls) =
  return $ replicate indent ' ' ++ "module " ++ show name ++ " where\n" ++ unlines (mapMaybe (externToPs (indent + 2) (subModule path name) env) decls)
externToPs _ _ _ _ = Nothing
