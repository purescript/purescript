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

import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Error
import Data.Either (rights, lefts)

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Declarations

addDataType :: ModuleName -> ProperName -> [String] -> [(ProperName, Maybe Type)] -> Kind -> Check ()
addDataType moduleName name args dctors ctorKind = do
  env <- getEnv
  putEnv $ env { types = M.insert (moduleName, name) (ctorKind, Data) (types env) }
  forM_ dctors $ \(dctor, maybeTy) ->
    rethrow (("Error in data constructor " ++ show name ++ ":\n") ++) $
      addDataConstructor moduleName name args dctor maybeTy

addDataConstructor :: ModuleName -> ProperName -> [String] -> ProperName -> Maybe Type -> Check ()
addDataConstructor moduleName name args dctor maybeTy = do
  env <- getEnv
  dataConstructorIsNotDefined moduleName dctor
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = maybe retTy (\ty -> Function [ty] retTy) maybeTy
  let polyType = mkForAll args dctorTy
  putEnv $ env { dataConstructors = M.insert (moduleName, dctor) (polyType, DataConstructor) (dataConstructors env) }

addTypeSynonym :: ModuleName -> ProperName -> [String] -> Type -> Kind -> Check ()
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  putEnv $ env { types = M.insert (moduleName, name) (kind, TypeSynonym) (types env)
               , typeSynonyms = M.insert (moduleName, name) (args, ty) (typeSynonyms env) }

typeIsNotDefined :: ModuleName -> ProperName -> Check ()
typeIsNotDefined moduleName name = do
  env <- getEnv
  guardWith (show name ++ " is already defined") $
    not $ M.member (moduleName, name) (types env)

dataConstructorIsNotDefined :: ModuleName -> ProperName -> Check ()
dataConstructorIsNotDefined moduleName dctor = do
  env <- getEnv
  guardWith (show dctor ++ " is already defined") $
    not $ M.member (moduleName, dctor) (dataConstructors env)

valueIsNotDefined :: ModuleName -> Ident -> Check ()
valueIsNotDefined moduleName name = do
  env <- getEnv
  case M.lookup (moduleName, name) (names env) of
    Just _ -> throwError $ show name ++ " is already defined"
    Nothing -> return ()

addValue :: ModuleName -> Ident -> Type -> Check ()
addValue moduleName name ty = do
  env <- getEnv
  putEnv (env { names = M.insert (moduleName, name) (ty, Value) (names env) })

typeCheckAll :: ModuleName -> [Declaration] -> Check ()
typeCheckAll _ [] = return ()
typeCheckAll moduleName (DataDeclaration name args dctors : rest) = do
  rethrow (("Error in type constructor " ++ show name ++ ":\n") ++) $ do
    typeIsNotDefined moduleName name
    ctorKind <- kindsOf moduleName name args (mapMaybe snd dctors)
    addDataType moduleName name args dctors ctorKind
  typeCheckAll moduleName rest
typeCheckAll moduleName (DataBindingGroupDeclaration tys : rest) = do
  rethrow (("Error in data binding group " ++ show (map (\(name, _, _) -> name) tys) ++ ":\n") ++) $ do
    forM_ tys $ \(name, _, _) ->
      typeIsNotDefined moduleName name
    ks <- kindsOfAll moduleName (map (\(name, args, dctors) -> (name, args, mapMaybe snd dctors)) tys)
    forM (zip tys ks) $ \((name, args, dctors), ctorKind) ->
      addDataType moduleName name args dctors ctorKind
  typeCheckAll moduleName rest
typeCheckAll moduleName (TypeSynonymDeclaration name args ty : rest) = do
  rethrow (("Error in type synonym " ++ show name ++ ":\n") ++) $ do
    typeIsNotDefined moduleName name
    kind <- kindsOf moduleName name args [ty]
    addTypeSynonym moduleName name args ty kind
  typeCheckAll moduleName rest
typeCheckAll _ (TypeDeclaration _ _ : _) = error "Type declarations should have been removed"
typeCheckAll moduleName (ValueDeclaration name [] Nothing val : rest) = do
  rethrow (("Error in declaration " ++ show name ++ ":\n") ++) $ do
    valueIsNotDefined moduleName name
    [ty] <- typesOf moduleName [(name, val)]
    addValue moduleName name ty
  typeCheckAll moduleName rest
typeCheckAll _ (ValueDeclaration _ _ _ _ : _) = error "Binders were not desugared"
typeCheckAll moduleName (BindingGroupDeclaration vals : rest) = do
  rethrow (("Error in binding group " ++ show (map fst vals) ++ ":\n") ++) $ do
    forM_ (map fst vals) $ \name ->
      valueIsNotDefined moduleName name
    tys <- typesOf moduleName vals
    forM (zip (map fst vals) tys) $ \(name, ty) ->
      addValue moduleName name ty
  typeCheckAll moduleName rest
typeCheckAll moduleName (ExternDataDeclaration name kind : rest) = do
  env <- getEnv
  guardWith (show name ++ " is already defined") $ not $ M.member (moduleName, name) (types env)
  putEnv $ env { types = M.insert (moduleName, name) (kind, TypeSynonym) (types env) }
  typeCheckAll moduleName rest
typeCheckAll moduleName (ExternDeclaration name _ ty : rest) = do
  rethrow (("Error in foreign import declaration " ++ show name ++ ":\n") ++) $ do
    env <- getEnv
    kind <- kindOf moduleName ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup (moduleName, name) (names env) of
      Just _ -> throwError $ show name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert (moduleName, name) (ty, Extern) (names env) })
  typeCheckAll moduleName rest
typeCheckAll moduleName (FixityDeclaration _ name : rest) = do
  typeCheckAll moduleName rest
  env <- getEnv
  guardWith ("Fixity declaration with no binding: " ++ name) $ M.member (moduleName, Op name) $ names env
typeCheckAll currentModule (ImportDeclaration moduleName idents : rest) = do
  env <- getEnv
  rethrow errorMessage $ do
    guardWith ("Module " ++ show moduleName ++ " does not exist") $ moduleExists env
    case idents of
      Nothing -> do
        shadowIdents (map snd $ filterModule (names env)) env
        shadowTypes (map snd $ filterModule (types env)) env
      Just idents' -> do
        shadowIdents (lefts idents') env
        shadowTypes (rights idents') env
  typeCheckAll currentModule rest
 where errorMessage = (("Error in import declaration " ++ show moduleName ++ ":\n") ++)
       filterModule = filter ((== moduleName) . fst) . M.keys
       moduleExists env = not (null (filterModule (names env))) || not (null (filterModule (types env)))
       shadowIdents idents' env =
         forM_ idents' $ \ident -> do
           case (moduleName, ident) `M.lookup` names env of
             Just (_, Alias _ _) -> return ()
             Just (pt, _) -> do
               guardWith (show currentModule ++ "." ++ show ident ++ " is already defined") $ (currentModule, ident) `M.notMember` names env
               modifyEnv (\e -> e { names = M.insert (currentModule, ident) (pt, Alias moduleName ident) (names e) })
             Nothing -> throwError (show moduleName ++ "." ++ show ident ++ " is undefined")
       shadowTypes pns env =
         forM_ pns $ \pn -> do
           case (moduleName, pn) `M.lookup` types env of
             Nothing -> throwError (show moduleName ++ "." ++ show pn ++ " is undefined")
             Just (_, DataAlias _ _) -> return ()
             Just (k, _) -> do
               guardWith (show currentModule ++ "." ++ show pn ++ " is already defined") $ (currentModule, pn) `M.notMember` types env
               modifyEnv (\e -> e { types = M.insert (currentModule, pn) (k, DataAlias moduleName pn) (types e) })
               let keys = map (snd . fst) . filter (\(_, (fn, _)) -> fn `constructs` pn) . M.toList . dataConstructors $ env
               forM_ keys $ \dctor -> do
                 case (moduleName, dctor) `M.lookup` dataConstructors env of
                   Just (_, Alias _ _) -> return ()
                   Just (ctorTy, _) -> do
                     guardWith (show currentModule ++ "." ++ show dctor ++ " is already defined") $ (currentModule, dctor) `M.notMember` dataConstructors env
                     modifyEnv (\e -> e { dataConstructors = M.insert (currentModule, dctor) (ctorTy, Alias moduleName (Ident (runProperName dctor))) (dataConstructors e) })
                   Nothing -> throwError (show moduleName ++ "." ++ show dctor ++ " is undefined")
       constructs (TypeConstructor (Qualified (Just mn) pn')) pn
         = mn == moduleName && pn' == pn
       constructs (ForAll _ ty) pn = ty `constructs` pn
       constructs (Function _ ty) pn = ty `constructs` pn
       constructs (TypeApp ty _) pn = ty `constructs` pn
       constructs fn _ = error $ "Invalid arguments to constructs: " ++ show fn

