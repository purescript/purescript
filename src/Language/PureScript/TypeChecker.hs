-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker
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

{-# LANGUAGE FlexibleInstances #-}

module Language.PureScript.TypeChecker (
    module T,
    typeCheckAll
) where

import Language.PureScript.TypeChecker.Monad as T
import Language.PureScript.TypeChecker.Kinds as T
import Language.PureScript.TypeChecker.Types as T
import Language.PureScript.TypeChecker.Synonyms as T

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Declarations

import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.Error

typeCheckAll :: [Declaration] -> Check ()
typeCheckAll [] = return ()
typeCheckAll (DataDeclaration name args dctors : rest) = do
  rethrow (("Error in type constructor " ++ name ++ ": ") ++) $ do
    env <- getEnv
    guardWith (name ++ " is already defined") $ not $ M.member name (types env)
    ctorKind <- kindsOf (Just name) args (mapMaybe snd dctors)
    putEnv $ env { types = M.insert name (ctorKind, Data) (types env) }
    forM_ dctors $ \(dctor, maybeTy) ->
      rethrow (("Error in data constructor " ++ name ++ ": ") ++) $ do
        env' <- getEnv
        guardWith (dctor ++ " is already defined") $ not $ M.member dctor (dataConstructors env')
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
typeCheckAll (TypeDeclaration name _ : _) = throwError $ "Orphan type declaration for " ++ show name
typeCheckAll (ValueDeclaration name val : rest) = do
  rethrow (("Error in declaration " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    case M.lookup name (names env) of
      Just ty -> throwError $ show name ++ " is already defined"
      Nothing -> do
        ty <- typeOf name val
        putEnv (env { names = M.insert name (ty, Value) (names env) })
  typeCheckAll rest
typeCheckAll (ExternDataDeclaration name kind : rest) = do
  env <- getEnv
  guardWith (name ++ " is already defined") $ not $ M.member name (types env)
  putEnv $ env { types = M.insert name (kind, TypeSynonym) (types env) }
  typeCheckAll rest
typeCheckAll (ExternDeclaration name ty : rest) = do
  rethrow (("Error in extern declaration " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    kind <- kindOf ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup name (names env) of
      Just _ -> throwError $ show name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert name (ty, Extern) (names env) })
  typeCheckAll rest
typeCheckAll (FixityDeclaration _ name : rest) = do
  typeCheckAll rest
  env <- getEnv
  guardWith ("Fixity declaration with no binding: " ++ name) $ M.member (Op name) $ names env
