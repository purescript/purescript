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
-- The top-level type checker, which checks all declarations in a module.
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
import Language.PureScript.Values
import Language.PureScript.Kinds
import Language.PureScript.Declarations
import Language.PureScript.Sugar.TypeClasses

addDataType :: ModuleName -> ProperName -> [String] -> [(ProperName, [Type])] -> Kind -> Check ()
addDataType moduleName name args dctors ctorKind = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) ctorKind (types env) }
  forM_ dctors $ \(dctor, tys) ->
    rethrow (("Error in data constructor " ++ show dctor ++ ":\n") ++) $
      addDataConstructor moduleName name args dctor tys

addDataConstructor :: ModuleName -> ProperName -> [String] -> ProperName -> [Type] -> Check ()
addDataConstructor moduleName name args dctor tys = do
  env <- getEnv
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = foldr function retTy tys
  let polyType = mkForAll args dctorTy
  putEnv $ env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) polyType (dataConstructors env) }

addTypeSynonym :: ModuleName -> ProperName -> [String] -> Type -> Kind -> Check ()
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) kind (types env)
               , typeSynonyms = M.insert (Qualified (Just moduleName) name) (args, ty) (typeSynonyms env) }

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

addTypeClassDictionaries :: [TypeClassDictionaryInScope] -> Check ()
addTypeClassDictionaries entries =
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = entries ++ (typeClassDictionaries . checkEnv $ st) } }

checkTypeClassInstance :: ModuleName -> Type -> Check ()
checkTypeClassInstance _ (TypeVar _) = return ()
checkTypeClassInstance m (TypeConstructor ctor) = do
  env <- getEnv
  when (ctor `M.member` typeSynonyms env) $ throwError "Type synonym instances are disallowed"
  return ()
checkTypeClassInstance m (TypeApp ty (TypeVar _)) = checkTypeClassInstance m ty
checkTypeClassInstance _ _ = throwError "Type class instance must be of the form T a1 ... an"

-- |
-- Type check all declarations in a module
--
-- At this point, many declarations will have been desugared, but it is still necessary to
--
--  * Kind-check all types and add them to the @Environment@
--
--  * Type-check all values and add them to the @Environment@
--
--  * Bring type class instances into scope
--
--  * Process module imports
--
typeCheckAll :: Maybe ModuleName -> ModuleName -> [Declaration] -> Check [Declaration]
typeCheckAll _ _ [] = return []
typeCheckAll mainModuleName moduleName (d@(DataDeclaration name args dctors) : rest) = do
  rethrow (("Error in type constructor " ++ show name ++ ":\n") ++) $ do
    ctorKind <- kindsOf moduleName name args (concatMap snd dctors)
    addDataType moduleName name args dctors ctorKind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(DataBindingGroupDeclaration tys) : rest) = do
  rethrow ("Error in data binding group:\n" ++) $ do
    let syns = mapMaybe toTypeSynonym tys
    let dataDecls = mapMaybe toDataDecl tys
    (syn_ks, data_ks) <- kindsOfAll moduleName syns (map (\(name, args, dctors) -> (name, args, concatMap snd dctors)) dataDecls)
    forM_ (zip dataDecls data_ks) $ \((name, args, dctors), ctorKind) ->
      addDataType moduleName name args dctors ctorKind
    forM_ (zip syns syn_ks) $ \((name, args, ty), kind) ->
      addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
  where
  toTypeSynonym (TypeSynonymDeclaration nm args ty) = Just (nm, args, ty)
  toTypeSynonym _ = Nothing
  toDataDecl (DataDeclaration nm args dctors) = Just (nm, args, dctors)
  toDataDecl _ = Nothing
typeCheckAll mainModuleName moduleName (d@(TypeSynonymDeclaration name args ty) : rest) = do
  rethrow (("Error in type synonym " ++ show name ++ ":\n") ++) $ do
    kind <- kindsOf moduleName name args [ty]
    addTypeSynonym moduleName name args ty kind
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (TypeDeclaration _ _ : _) = error "Type declarations should have been removed"
typeCheckAll mainModuleName moduleName (ValueDeclaration name [] Nothing val : rest) = do
  d <- rethrow (("Error in declaration " ++ show name ++ ":\n") ++) $ do
    valueIsNotDefined moduleName name
    [(_, (val', ty))] <- typesOf mainModuleName moduleName [(name, val)]
    addValue moduleName name ty
    return $ ValueDeclaration name [] Nothing val'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll _ _ (ValueDeclaration{} : _) = error "Binders were not desugared"
typeCheckAll mainModuleName moduleName (BindingGroupDeclaration vals : rest) = do
  d <- rethrow (("Error in binding group " ++ show (map fst vals) ++ ":\n") ++) $ do
    forM_ (map fst vals) $ \name ->
      valueIsNotDefined moduleName name
    tys <- typesOf mainModuleName moduleName vals
    vals' <- forM (zip (map fst vals) (map snd tys)) $ \(name, (val, ty)) -> do
      addValue moduleName name ty
      return (name, val)
    return $ BindingGroupDeclaration vals'
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(ExternDataDeclaration name kind) : rest) = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) kind (types env) }
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(ExternDeclaration importTy name _ ty) : rest) = do
  rethrow (("Error in foreign import declaration " ++ show name ++ ":\n") ++) $ do
    env <- getEnv
    kind <- kindOf moduleName ty
    guardWith "Expected kind *" $ kind == Star
    case M.lookup (moduleName, name) (names env) of
      Just _ -> throwError $ show name ++ " is already defined"
      Nothing -> putEnv (env { names = M.insert (moduleName, name) (ty, Extern importTy) (names env) })
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(FixityDeclaration _ name) : rest) = do
  ds <- typeCheckAll mainModuleName moduleName rest
  env <- getEnv
  guardWith ("Fixity declaration with no binding: " ++ name) $ M.member (moduleName, Op name) $ names env
  return $ d : ds
typeCheckAll mainModuleName currentModule (d@(ImportDeclaration moduleName idents) : rest) = do
  env <- getEnv
  let instances = filter (\tcd ->
                    let Qualified (Just mn) _ = tcdName tcd in
                    moduleName == mn && tcdType tcd == TCDRegular
                  ) (typeClassDictionaries env)
  forM_ instances $ \tcd -> do
    let (Qualified _ ident) = tcdName tcd
    addTypeClassDictionaries [tcd { tcdName = Qualified (Just currentModule) ident, tcdType = TCDAlias (tcdName tcd) }]
  ds <- typeCheckAll mainModuleName currentModule rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@TypeClassDeclaration{} : rest) = do
  env <- getEnv
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
typeCheckAll mainModuleName moduleName (d@(TypeInstanceDeclaration deps className tys _) : rest) = do
  env <- getEnv
  dictName <- Check . lift $ mkDictionaryValueName moduleName className tys
  mapM_ (checkTypeClassInstance moduleName) tys
  forM_ deps $ mapM_ (checkTypeClassInstance moduleName) . snd
  addTypeClassDictionaries [TypeClassDictionaryInScope (Qualified (Just moduleName) dictName) className tys (Just deps) TCDRegular]
  ds <- typeCheckAll mainModuleName moduleName rest
  return $ d : ds
