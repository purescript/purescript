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
    typeCheckModule
) where

import Language.PureScript.TypeChecker.Monad as T
import Language.PureScript.TypeChecker.Kinds as T
import Language.PureScript.TypeChecker.Types as T
import Language.PureScript.TypeChecker.Synonyms as T

import Data.Maybe
import Data.List (nub, (\\), find, intercalate)
import Data.Monoid ((<>))
import Data.Foldable (for_)
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Error

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Declarations
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Traversals

addDataType :: ModuleName -> DataDeclType -> ProperName -> [String] -> [DataConstructor] -> Kind -> Check ()
addDataType moduleName dtype name args dctors ctorKind = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (ctorKind, DataType args (map stripComment dctors)) (types env) }
  forM_ dctors $ \(dctor, tys, _) ->
    rethrow (strMsg ("Error in data constructor " ++ show dctor) <>) $
      addDataConstructor moduleName dtype name args dctor tys

                 where stripComment (a, b, _) = (a, b)

addDataConstructor :: ModuleName -> DataDeclType -> ProperName -> [String] -> ProperName -> [Type] -> Check ()
addDataConstructor moduleName dtype name args dctor tys = do
  env <- getEnv
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = foldr function retTy tys
  let polyType = mkForAll args dctorTy
  putEnv $ env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) (dtype, name, polyType) (dataConstructors env) }

addTypeSynonym :: ModuleName -> ProperName -> [String] -> Type -> Kind -> Check ()
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (kind, TypeSynonym) (types env)
               , typeSynonyms = M.insert (Qualified (Just moduleName) name) (args, ty) (typeSynonyms env) }

valueIsNotDefined :: ModuleName -> Ident -> Check ()
valueIsNotDefined moduleName name = do
  env <- getEnv
  case M.lookup (moduleName, name) (names env) of
    Just _ -> throwError . strMsg $ show name ++ " is already defined"
    Nothing -> return ()

addValue :: ModuleName -> Ident -> Type -> NameKind -> Check ()
addValue moduleName name ty nameKind = do
  env <- getEnv
  putEnv (env { names = M.insert (moduleName, name) (ty, nameKind, Defined) (names env) })

addTypeClass :: ModuleName -> ProperName -> [String] -> [(Qualified ProperName, [Type])] -> [Declaration] -> Check ()
addTypeClass moduleName pn args implies ds =
  let members = map toPair ds in
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClasses = M.insert (Qualified (Just moduleName) pn) (args, members, implies) (typeClasses . checkEnv $ st) } }
  where
  toPair (TypeDeclaration ident ty) = (ident, ty)
  toPair (PositionedDeclaration _ d) = toPair d
  toPair (DocStringDeclaration _ (Just d)) = toPair d
  toPair _ = error "Invalid declaration in TypeClassDeclaration"

addTypeClassDictionaries :: [TypeClassDictionaryInScope] -> Check ()
addTypeClassDictionaries entries =
  let mentries = M.fromList [ ((canonicalizeDictionary entry, mn), entry) | entry@TypeClassDictionaryInScope{ tcdName = Qualified mn _ }  <- entries ]
  in modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = (typeClassDictionaries . checkEnv $ st) `M.union` mentries } }

checkDuplicateTypeArguments :: [String] -> Check ()
checkDuplicateTypeArguments args = for_ firstDup $ \dup ->
  throwError . strMsg $ "Duplicate type argument '" ++ dup ++ "'"
  where
  firstDup :: Maybe String
  firstDup = listToMaybe $ args \\ nub args

checkTypeClassInstance :: ModuleName -> Type -> Check ()
checkTypeClassInstance _ (TypeVar _) = return ()
checkTypeClassInstance _ (TypeConstructor ctor) = do
  env <- getEnv
  when (ctor `M.member` typeSynonyms env) . throwError . strMsg $ "Type synonym instances are disallowed"
  return ()
checkTypeClassInstance m (TypeApp t1 t2) = checkTypeClassInstance m t1 >> checkTypeClassInstance m t2
checkTypeClassInstance _ ty = throwError $ mkErrorStack "Type class instance head is invalid." (Just (TypeError ty))

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
typeCheckAll :: Maybe ModuleName -> ModuleName -> [DeclarationRef] -> [Declaration] -> Check [Declaration]
typeCheckAll mainModuleName moduleName exps = go
  where
  go :: [Declaration] -> Check [Declaration]
  go [] = return []
  go (d@(DataDeclaration dtype name args dctors) : rest) = do
    rethrow (strMsg ("Error in type constructor " ++ show name) <>) $ do
      when (dtype == Newtype) $ checkNewtype dctors
      checkDuplicateTypeArguments args
      ctorKind <- kindsOf True moduleName name args (concatMap snd3 dctors)
      addDataType moduleName dtype name args dctors ctorKind
    ds <- go rest
    return $ d : ds
    where
    checkNewtype :: [DataConstructor] -> Check ()
    checkNewtype [(_, [_], _)] = return ()
    checkNewtype [(_, _, _)] = throwError . strMsg $ "newtypes constructors must have a single argument"
    checkNewtype _ = throwError . strMsg $ "newtypes must have a single constructor"
  go (d@(DataBindingGroupDeclaration tys) : rest) = do
    rethrow (strMsg "Error in data binding group" <>) $ do
      let syns = mapMaybe toTypeSynonym tys
      let dataDecls = mapMaybe toDataDecl tys
      (syn_ks, data_ks) <- kindsOfAll moduleName syns (map (\(_, name, args, dctors) -> (name, args, concatMap snd3 dctors)) dataDecls)
      forM_ (zip dataDecls data_ks) $ \((dtype, name, args, dctors), ctorKind) -> do
        checkDuplicateTypeArguments args
        addDataType moduleName dtype name args dctors ctorKind
      forM_ (zip syns syn_ks) $ \((name, args, ty), kind) -> do
        checkDuplicateTypeArguments args
        addTypeSynonym moduleName name args ty kind
    ds <- go rest
    return $ d : ds
    where
    toTypeSynonym (TypeSynonymDeclaration nm args ty) = Just (nm, args, ty)
    toTypeSynonym (PositionedDeclaration _ d') = toTypeSynonym d'
    toTypeSynonym (DocStringDeclaration _ (Just d')) = toTypeSynonym d'
    toTypeSynonym _ = Nothing
    toDataDecl (DataDeclaration dtype nm args dctors) = Just (dtype, nm, args, dctors)
    toDataDecl (PositionedDeclaration _ d') = toDataDecl d'
    toDataDecl (DocStringDeclaration _ (Just d')) = toDataDecl d'
    toDataDecl _ = Nothing
  go (d@(TypeSynonymDeclaration name args ty) : rest) = do
    rethrow (strMsg ("Error in type synonym " ++ show name) <>) $ do
      checkDuplicateTypeArguments args
      kind <- kindsOf False moduleName name args [ty]
      addTypeSynonym moduleName name args ty kind
    ds <- go rest
    return $ d : ds
  go (TypeDeclaration _ _ : _) = error "Type declarations should have been removed"
  go (ValueDeclaration name nameKind [] Nothing val : rest) = do
    d <- rethrow (strMsg ("Error in declaration " ++ show name) <>) $ do
      valueIsNotDefined moduleName name
      [(_, (val', ty))] <- typesOf mainModuleName moduleName [(name, val)]
      addValue moduleName name ty nameKind
      return $ ValueDeclaration name nameKind [] Nothing val'
    ds <- go rest
    return $ d : ds
  go (ValueDeclaration{} : _) = error "Binders were not desugared"
  go (BindingGroupDeclaration vals : rest) = do
    d <- rethrow (strMsg ("Error in binding group " ++ show (map (\(ident, _, _) -> ident) vals)) <>) $ do
      forM_ (map (\(ident, _, _) -> ident) vals) $ \name ->
        valueIsNotDefined moduleName name
      tys <- typesOf mainModuleName moduleName $ map (\(ident, _, ty) -> (ident, ty)) vals
      vals' <- forM (zipWith (\(name, nameKind, _) (_, (val, ty)) -> (name, val, nameKind, ty)) vals tys) $ \(name, val, nameKind, ty) -> do
        addValue moduleName name ty nameKind
        return (name, nameKind, val)
      return $ BindingGroupDeclaration vals'
    ds <- go rest
    return $ d : ds
  go (d@(ExternDataDeclaration name kind) : rest) = do
    env <- getEnv
    putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (kind, ExternData) (types env) }
    ds <- go rest
    return $ d : ds
  go (d@(ExternDeclaration importTy name _ ty) : rest) = do
    rethrow (strMsg ("Error in foreign import declaration " ++ show name) <>) $ do
      env <- getEnv
      kind <- kindOf moduleName ty
      guardWith (strMsg "Expected kind *") $ kind == Star
      case M.lookup (moduleName, name) (names env) of
        Just _ -> throwError . strMsg $ show name ++ " is already defined"
        Nothing -> putEnv (env { names = M.insert (moduleName, name) (ty, Extern importTy, Defined) (names env) })
    ds <- go rest
    return $ d : ds
  go (d@(FixityDeclaration _ name) : rest) = do
    ds <- go rest
    env <- getEnv
    guardWith (strMsg ("Fixity declaration with no binding: " ++ name)) $ M.member (moduleName, Op name) $ names env
    return $ d : ds
  go (d@(ImportDeclaration importedModule _ _) : rest) = do
    tcds <- getTypeClassDictionaries
    let instances = filter (\tcd -> let Qualified (Just mn) _ = tcdName tcd in importedModule == mn) tcds
    addTypeClassDictionaries [ tcd { tcdName = Qualified (Just moduleName) ident, tcdType = TCDAlias (canonicalizeDictionary tcd) }
                             | tcd <- instances
                             , tcdExported tcd
                             , let (Qualified _ ident) = tcdName tcd
                             ]
    ds <- go rest
    return $ d : ds
  go (d@(TypeClassDeclaration pn args implies tys) : rest) = do
    addTypeClass moduleName pn args implies tys
    ds <- go rest
    return $ d : ds
  go (TypeInstanceDeclaration dictName deps className tys _ : rest) =
    go (ExternInstanceDeclaration dictName deps className tys : rest)
  go (d@(ExternInstanceDeclaration dictName deps className tys) : rest) = do
    mapM_ (checkTypeClassInstance moduleName) tys
    forM_ deps $ mapM_ (checkTypeClassInstance moduleName) . snd
    addTypeClassDictionaries [TypeClassDictionaryInScope (Qualified (Just moduleName) dictName) className tys (Just deps) TCDRegular isInstanceExported]
    ds <- go rest
    return $ d : ds
    where
    isInstanceExported :: Bool
    isInstanceExported = any exportsInstance exps

    exportsInstance :: DeclarationRef -> Bool
    exportsInstance (TypeInstanceRef name) | name == dictName = True
    exportsInstance (PositionedDeclarationRef _ r) = exportsInstance r
    exportsInstance _ = False

  go (PositionedDeclaration pos d : rest) =
    rethrowWithPosition pos $ do
      (d' : rest') <- go (d : rest)
      return (PositionedDeclaration pos d' : rest')
  go (DocStringDeclaration _ (Just d) : rest) = go (d : rest)
  go (DocStringDeclaration _ Nothing : rest) = go rest

-- |
-- Type check an entire module and ensure all types and classes defined within the module that are
-- required by exported members are also exported.
--
typeCheckModule :: Maybe ModuleName -> Module -> Check Module
typeCheckModule _ (Module _ _ Nothing) = error "exports should have been elaborated"
typeCheckModule mainModuleName (Module mn decls (Just exps)) = do
  modify (\s -> s { checkCurrentModule = Just mn })
  decls' <- typeCheckAll mainModuleName mn exps decls
  forM_ exps $ \e -> do
    checkTypesAreExported e
    checkClassMembersAreExported e
    checkClassesAreExported e
  return $ Module mn decls' (Just exps)
  where

  checkMemberExport :: (Show a) => String -> (Type -> [a]) -> (a -> Bool) -> DeclarationRef -> Check ()
  checkMemberExport thing extract test (ValueRef name) = do
    ty <- lookupVariable mn (Qualified (Just mn) name)
    case find test (extract ty) of
      Just hiddenType -> throwError . strMsg $
        "Error in module '" ++ show mn ++ "':\n\
        \Exporting declaration '" ++ show name ++ "' requires " ++ thing ++ " '" ++ show hiddenType ++ "' to be exported as well"
      Nothing -> return ()
  checkMemberExport _ _ _ _ = return ()

  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> Check ()
  checkTypesAreExported = checkMemberExport "type" findTcons isTconHidden
    where
    findTcons :: Type -> [ProperName]
    findTcons = everythingOnTypes (++) go
      where
      go (TypeConstructor (Qualified (Just mn') name)) | mn' == mn = [name]
      go _ = []
    isTconHidden :: ProperName -> Bool
    isTconHidden tyName = all go exps
      where
      go (TypeRef tyName' _) = tyName' /= tyName
      go _ = True

  -- Check that all the classes defined in the current module that appear in member types have also
  -- been exported from the module
  checkClassesAreExported :: DeclarationRef -> Check ()
  checkClassesAreExported = checkMemberExport "class" findClasses isClassHidden
    where
    findClasses :: Type -> [ProperName]
    findClasses = everythingOnTypes (++) go
      where
      go (ConstrainedType cs _) = mapMaybe (extractCurrentModuleClass . fst) cs
      go _ = []
    extractCurrentModuleClass :: Qualified ProperName -> Maybe ProperName
    extractCurrentModuleClass (Qualified (Just mn') name) | mn == mn' = Just name
    extractCurrentModuleClass _ = Nothing
    isClassHidden :: ProperName -> Bool
    isClassHidden clsName = all go exps
      where
      go (TypeClassRef clsName') = clsName' /= clsName
      go _ = True

  checkClassMembersAreExported :: DeclarationRef -> Check ()
  checkClassMembersAreExported (TypeClassRef name) = do
    let members = ValueRef `map` head (mapMaybe findClassMembers decls)
    let missingMembers = members \\ exps
    unless (null missingMembers) $
      throwError . strMsg $
        "Error in module '" ++ show mn ++ "':\n\
        \Class '" ++ show name ++ "' is exported but is missing member exports for '" ++ intercalate "', '" (map (show . runValueRef) missingMembers) ++ "'"
    where
    runValueRef :: DeclarationRef -> Ident
    runValueRef (ValueRef refName) = refName
    runValueRef _ = error "non-ValueRef passed to runValueRef"
    findClassMembers :: Declaration -> Maybe [Ident]
    findClassMembers (TypeClassDeclaration name' _ _ ds) | name == name' = Just $ map extractMemberName ds
    findClassMembers (PositionedDeclaration _ d) = findClassMembers d
    findClassMembers (DocStringDeclaration _ (Just d)) = findClassMembers d
    findClassMembers _ = Nothing
    extractMemberName :: Declaration -> Ident
    extractMemberName (PositionedDeclaration _ d) = extractMemberName d
    extractMemberName (DocStringDeclaration _ (Just d)) = extractMemberName d
    extractMemberName (TypeDeclaration memberName _) = memberName
    extractMemberName _ = error "Unexpected declaration in typeclass member list"
  checkClassMembersAreExported _ = return ()
