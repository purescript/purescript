-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker
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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module PureScript.TypeChecker (
    module PureScript.TypeChecker.Monad,
    module PureScript.TypeChecker.Kinds,
    module PureScript.TypeChecker.Types,
    typeCheckAll,
) where

import PureScript.TypeChecker.Monad
import PureScript.TypeChecker.Kinds
import PureScript.TypeChecker.Types
import PureScript.TypeChecker.Synonyms

import Data.List
import Data.Maybe
import Data.Function

import PureScript.Values
import PureScript.Types
import PureScript.Kinds
import PureScript.Declarations

import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as M

typeCheckAll :: [Declaration] -> Check ()
typeCheckAll [] = return ()
typeCheckAll (DataDeclaration name args dctors : rest) = do
  rethrow (("Error in type constructor " ++ name ++ ": ") ++) $ do
    env <- getEnv
    guardWith (name ++ " is already defined") $ not $ M.member name (types env)
    ctorKind <- kindsOf (Just name) args (catMaybes $ map snd dctors)
    putEnv $ env { types = M.insert name (ctorKind, Data) (types env) }
    flip mapM_ dctors $ \(dctor, maybeTy) ->
      rethrow (("Error in data constructor " ++ name ++ ": ") ++) $ do
        env' <- getEnv
        guardWith (dctor ++ " is already defined") $ not $ flip M.member (names env') dctor
        let retTy = foldl TypeApp (TypeConstructor name) (map TypeVar args)
        let dctorTy = maybe retTy (\ty -> Function [ty] retTy) maybeTy
        let polyType = PolyType args dctorTy
        putEnv $ env' { dataConstructors = M.insert dctor polyType (dataConstructors env') }
  typeCheckAll rest
typeCheckAll (TypeSynonymDeclaration name args ty : rest) = do
  rethrow (("Error in type synonym " ++ name ++ ": ") ++) $ do
    env <- getEnv
    guardWith (name ++ " is already defined") $ not $ M.member name (types env)
    kind <- kindsOf (Just name) args [ty]
    putEnv $ env { types = M.insert name (kind, TypeSynonym) (types env)
                 , typeSynonyms = M.insert name (args, ty) (typeSynonyms env) }
  typeCheckAll rest
typeCheckAll (TypeDeclaration name ty : ValueDeclaration name' val : rest) | name == name' =
  typeCheckAll (ValueDeclaration name (TypedValue val ty) : rest)
typeCheckAll (TypeDeclaration name _ : _) = throwError $ "Orphan type declaration for " ++ name
typeCheckAll (ValueDeclaration name val : rest) = do
  rethrow (("Error in declaration " ++ name ++ ": ") ++) $ do
    env <- getEnv
    case M.lookup name (names env) of
      Just ty -> throwError $ name ++ " is already defined"
      Nothing -> do
        ty <- typeOf name val
        putEnv (env { names = M.insert name (ty, Value) (names env) })
  typeCheckAll rest
typeCheckAll (ExternDeclaration name ty : rest) = do
  rethrow (("Error in extern declaration " ++ name ++ ": ") ++) $ do
    env <- getEnv
    kind <- kindOf ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup name (names env) of
      Just _ -> throwError $ name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert name (ty, Extern) (names env) })
  typeCheckAll rest
