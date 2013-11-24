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
  rethrow (("Error in type constructor " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    guardWith (show name ++ " is already defined") $ not $ M.member (modulePath, name) (types env)
    ctorKind <- kindsOf (Just name) args (mapMaybe snd dctors)
    putEnv $ env { types = M.insert (modulePath, name) (ctorKind, Data) (types env) }
    forM_ dctors $ \(dctor, maybeTy) ->
      rethrow (("Error in data constructor " ++ show name ++ ": ") ++) $ do
        env' <- getEnv
        guardWith (show dctor ++ " is already defined") $ not $ M.member (modulePath, dctor) (dataConstructors env')
        let retTy = foldl TypeApp (TypeConstructor (Qualified modulePath name)) (map TypeVar args)
        let dctorTy = maybe retTy (\ty -> Function [ty] retTy) maybeTy
        let polyType = mkForAll args dctorTy
        putEnv $ env' { dataConstructors = M.insert (modulePath, dctor) polyType (dataConstructors env') }
  typeCheckAll rest
typeCheckAll (TypeSynonymDeclaration name args ty : rest) = do
  rethrow (("Error in type synonym " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    guardWith (show name ++ " is already defined") $ not $ M.member (modulePath, name) (types env)
    kind <- kindsOf (Just name) args [ty]
    putEnv $ env { types = M.insert (modulePath, name) (kind, TypeSynonym) (types env)
                 , typeSynonyms = M.insert (modulePath, name) (args, ty) (typeSynonyms env) }
  typeCheckAll rest
typeCheckAll (TypeDeclaration name ty : ValueDeclaration name' val : rest) | name == name' =
  typeCheckAll (ValueDeclaration name (TypedValue val ty) : rest)
typeCheckAll (TypeDeclaration name _ : _) = throwError $ "Orphan type declaration for " ++ show name
typeCheckAll (ValueDeclaration name val : rest) = do
  rethrow (("Error in declaration " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    case M.lookup (modulePath, name) (names env) of
      Just ty -> throwError $ show name ++ " is already defined"
      Nothing -> do
        ty <- typeOf (Just name) val
        putEnv (env { names = M.insert (modulePath, name) (ty, Value) (names env) })
  typeCheckAll rest
typeCheckAll (ExternDataDeclaration name kind : rest) = do
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  guardWith (show name ++ " is already defined") $ not $ M.member (modulePath, name) (types env)
  putEnv $ env { types = M.insert (modulePath, name) (kind, TypeSynonym) (types env) }
  typeCheckAll rest
typeCheckAll (ExternMemberDeclaration member name ty : rest) = do
  rethrow (("Error in foreign import member declaration " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    kind <- kindOf ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup (modulePath, name) (names env) of
      Just _ -> throwError $ show name ++ " is already defined"
      Nothing -> case ty of
        (ForAll _ (Function [_] _)) -> do
          putEnv (env { names = M.insert (modulePath, name) (ty, Extern) (names env)
                      , members = M.insert (modulePath, name) member (members env) })
        _ -> throwError "Foreign member declarations must have function types, with an single argument."
  typeCheckAll rest
typeCheckAll (ExternDeclaration name ty : rest) = do
  rethrow (("Error in foreign import declaration " ++ show name ++ ": ") ++) $ do
    env <- getEnv
    modulePath <- checkModulePath `fmap` get
    kind <- kindOf ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup (modulePath, name) (names env) of
      Just _ -> throwError $ show name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert (modulePath, name) (ty, Extern) (names env) })
  typeCheckAll rest
typeCheckAll (FixityDeclaration _ name : rest) = do
  typeCheckAll rest
  env <- getEnv
  modulePath <- checkModulePath `fmap` get
  guardWith ("Fixity declaration with no binding: " ++ name) $ M.member (modulePath, Op name) $ names env
typeCheckAll (ModuleDeclaration name decls : rest) = do
  withModule name $ typeCheckAll decls
  typeCheckAll rest
typeCheckAll (ImportDeclaration modulePath idents : rest) = do
  env <- getEnv
  currentModule <- checkModulePath `fmap` get
  rethrow errorMessage $ do
    guardWith ("Module " ++ show modulePath ++ " does not exist") $ moduleExists env
    case idents of
      Nothing     -> bindIdents (map snd $ filterModule env) currentModule env
      Just idents -> bindIdents idents currentModule env
  typeCheckAll rest
 where errorMessage = (("Error in import declaration " ++ show modulePath ++ ": ") ++)
       filterModule = filter ((== modulePath) . fst) . M.keys . names
       moduleExists env = not $ null $ filterModule env
       bindIdents idents currentModule env =
         forM_ idents $ \ident -> do
           guardWith (show currentModule ++ "." ++ show ident ++ " is already defined") $ (currentModule, ident) `M.notMember` names env
           case (modulePath, ident) `M.lookup` names env of
             Just (pt, _) -> modifyEnv (\e -> e { names = M.insert (currentModule, ident) (pt, Alias modulePath ident) (names e) })
             Nothing -> throwError (show modulePath ++ "." ++ show ident ++ " is undefined")
