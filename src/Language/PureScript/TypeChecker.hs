-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
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
import Data.List (nub, (\\))
import Data.Foldable (for_)

import qualified Data.Map as M

import Control.Applicative ((<$>), (<*))
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.AST
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Environment
import Language.PureScript.Errors

addDataType :: ModuleName -> DataDeclType -> ProperName -> [(String, Maybe Kind)] -> [(ProperName, [Type])] -> Kind -> Check ()
addDataType moduleName dtype name args dctors ctorKind = do
  env <- getEnv
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (ctorKind, DataType args dctors) (types env) }
  forM_ dctors $ \(dctor, tys) ->
    rethrow (onErrorMessages (ErrorInDataConstructor dctor)) $
      addDataConstructor moduleName dtype name (map fst args) dctor tys

addDataConstructor :: ModuleName -> DataDeclType -> ProperName -> [String] -> ProperName -> [Type] -> Check ()
addDataConstructor moduleName dtype name args dctor tys = do
  env <- getEnv
  mapM_ checkTypeSynonyms tys
  let retTy = foldl TypeApp (TypeConstructor (Qualified (Just moduleName) name)) (map TypeVar args)
  let dctorTy = foldr function retTy tys
  let polyType = mkForAll args dctorTy
  let fields = [Ident ("value" ++ show n) | n <- [0..(length tys - 1)]]
  putEnv $ env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) (dtype, name, polyType, fields) (dataConstructors env) }

addTypeSynonym :: ModuleName -> ProperName -> [(String, Maybe Kind)] -> Type -> Kind -> Check ()
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  checkTypeSynonyms ty
  putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (kind, TypeSynonym) (types env)
               , typeSynonyms = M.insert (Qualified (Just moduleName) name) (args, ty) (typeSynonyms env) }

valueIsNotDefined :: ModuleName -> Ident -> Check ()
valueIsNotDefined moduleName name = do
  env <- getEnv
  case M.lookup (moduleName, name) (names env) of
    Just _ -> throwError . errorMessage $ RedefinedIdent name
    Nothing -> return ()

addValue :: ModuleName -> Ident -> Type -> NameKind -> Check ()
addValue moduleName name ty nameKind = do
  env <- getEnv
  putEnv (env { names = M.insert (moduleName, name) (ty, nameKind, Defined) (names env) })

addTypeClass :: ModuleName -> ProperName -> [(String, Maybe Kind)] -> [Constraint] -> [Declaration] -> Check ()
addTypeClass moduleName pn args implies ds =
  let members = map toPair ds in
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClasses = M.insert (Qualified (Just moduleName) pn) (args, members, implies) (typeClasses . checkEnv $ st) } }
  where
  toPair (TypeDeclaration ident ty) = (ident, ty)
  toPair (PositionedDeclaration _ _ d) = toPair d
  toPair _ = error "Invalid declaration in TypeClassDeclaration"

addTypeClassDictionaries :: Maybe ModuleName -> M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope) -> Check ()
addTypeClassDictionaries mn entries =
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = insertState st } }
  where insertState st = M.insertWith (M.unionWith M.union) mn entries (typeClassDictionaries . checkEnv $ st)

checkDuplicateTypeArguments :: [String] -> Check ()
checkDuplicateTypeArguments args = for_ firstDup $ \dup ->
  throwError . errorMessage $ DuplicateTypeArgument dup
  where
  firstDup :: Maybe String
  firstDup = listToMaybe $ args \\ nub args

checkTypeClassInstance :: ModuleName -> Type -> Check ()
checkTypeClassInstance _ (TypeVar _) = return ()
checkTypeClassInstance _ (TypeConstructor ctor) = do
  env <- getEnv
  when (ctor `M.member` typeSynonyms env) . throwError . errorMessage $ TypeSynonymInstance
  return ()
checkTypeClassInstance m (TypeApp t1 t2) = checkTypeClassInstance m t1 >> checkTypeClassInstance m t2
checkTypeClassInstance _ ty = throwError . errorMessage $ InvalidInstanceHead ty

-- |
-- Check that type synonyms are fully-applied in a type
--
checkTypeSynonyms :: Type -> Check ()
checkTypeSynonyms = void . replaceAllTypeSynonyms

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
typeCheckAll mainModuleName moduleName exps ds = mapM go ds <* mapM_ checkOrphanFixities ds
  where
  go :: Declaration -> Check Declaration
  go (DataDeclaration dtype name args dctors) = do
    rethrow (onErrorMessages (ErrorInTypeConstructor name)) $ do
      when (dtype == Newtype) $ checkNewtype dctors
      checkDuplicateTypeArguments $ map fst args
      ctorKind <- kindsOf True moduleName name args (concatMap snd dctors)
      let args' = args `withKinds` ctorKind
      addDataType moduleName dtype name args' dctors ctorKind
    return $ DataDeclaration dtype name args dctors
    where
    checkNewtype :: [(ProperName, [Type])] -> Check ()
    checkNewtype [(_, [_])] = return ()
    checkNewtype [(_, _)] = throwError . errorMessage $ InvalidNewtype
    checkNewtype _ = throwError . errorMessage $ InvalidNewtype
  go (d@(DataBindingGroupDeclaration tys)) = do
    rethrow (onErrorMessages ErrorInDataBindingGroup) $ do
      let syns = mapMaybe toTypeSynonym tys
      let dataDecls = mapMaybe toDataDecl tys
      (syn_ks, data_ks) <- kindsOfAll moduleName syns (map (\(_, name, args, dctors) -> (name, args, concatMap snd dctors)) dataDecls)
      forM_ (zip dataDecls data_ks) $ \((dtype, name, args, dctors), ctorKind) -> do
        checkDuplicateTypeArguments $ map fst args
        let args' = args `withKinds` ctorKind
        addDataType moduleName dtype name args' dctors ctorKind
      forM_ (zip syns syn_ks) $ \((name, args, ty), kind) -> do
        checkDuplicateTypeArguments $ map fst args
        let args' = args `withKinds` kind
        addTypeSynonym moduleName name args' ty kind
    return d
    where
    toTypeSynonym (TypeSynonymDeclaration nm args ty) = Just (nm, args, ty)
    toTypeSynonym (PositionedDeclaration _ _ d') = toTypeSynonym d'
    toTypeSynonym _ = Nothing
    toDataDecl (DataDeclaration dtype nm args dctors) = Just (dtype, nm, args, dctors)
    toDataDecl (PositionedDeclaration _ _ d') = toDataDecl d'
    toDataDecl _ = Nothing
  go (TypeSynonymDeclaration name args ty) = do
    rethrow (onErrorMessages (ErrorInTypeSynonym name)) $ do
      checkDuplicateTypeArguments $ map fst args
      kind <- kindsOf False moduleName name args [ty]
      let args' = args `withKinds` kind
      addTypeSynonym moduleName name args' ty kind
    return $ TypeSynonymDeclaration name args ty
  go (TypeDeclaration{}) = error "Type declarations should have been removed"
  go (ValueDeclaration name nameKind [] (Right val)) =
    rethrow (onErrorMessages (ErrorInValueDeclaration name)) $ do
      valueIsNotDefined moduleName name
      [(_, (val', ty))] <- typesOf mainModuleName moduleName [(name, val)]
      addValue moduleName name ty nameKind
      return $ ValueDeclaration name nameKind [] $ Right val'
  go (ValueDeclaration{}) = error "Binders were not desugared"
  go (BindingGroupDeclaration vals) =
    rethrow (onErrorMessages (ErrorInBindingGroup (map (\(ident, _, _) -> ident) vals))) $ do
      forM_ (map (\(ident, _, _) -> ident) vals) $ \name ->
        valueIsNotDefined moduleName name
      tys <- typesOf mainModuleName moduleName $ map (\(ident, _, ty) -> (ident, ty)) vals
      vals' <- forM [ (name, val, nameKind, ty)
                    | (name, nameKind, _) <- vals
                    , (name', (val, ty)) <- tys
                    , name == name'
                    ] $ \(name, val, nameKind, ty) -> do
        addValue moduleName name ty nameKind
        return (name, nameKind, val)
      return $ BindingGroupDeclaration vals'
  go (d@(ExternDataDeclaration name kind)) = do
    env <- getEnv
    putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (kind, ExternData) (types env) }
    return d
  go (d@(ExternDeclaration name ty)) = do
    rethrow (onErrorMessages (ErrorInForeignImport name)) $ do
      env <- getEnv
      kind <- kindOf moduleName ty
      guardWith (errorMessage (ExpectedType kind)) $ kind == Star
      case M.lookup (moduleName, name) (names env) of
        Just _ -> throwError . errorMessage $ RedefinedIdent name
        Nothing -> putEnv (env { names = M.insert (moduleName, name) (ty, External, Defined) (names env) })
    return d
  go (d@(FixityDeclaration{})) = return d
  go (d@(ImportDeclaration importedModule _ _)) = do
    instances <- lookupTypeClassDictionaries $ Just importedModule
    addTypeClassDictionaries (Just moduleName) instances
    return d
  go (d@(TypeClassDeclaration pn args implies tys)) = do
    addTypeClass moduleName pn args implies tys
    return d
  go (d@(TypeInstanceDeclaration dictName deps className tys _)) =
    goInstance d dictName deps className tys
  go (d@(ExternInstanceDeclaration dictName deps className tys)) =
    goInstance d dictName deps className tys
  go (PositionedDeclaration pos com d) =
    rethrowWithPosition pos $ PositionedDeclaration pos com <$> go d

  checkOrphanFixities :: Declaration -> Check ()
  checkOrphanFixities (FixityDeclaration _ name) = do
    env <- getEnv
    guardWith (errorMessage (OrphanFixityDeclaration name)) $ M.member (moduleName, Op name) $ names env
  checkOrphanFixities (PositionedDeclaration pos _ d) =
    rethrowWithPosition pos $ checkOrphanFixities d
  checkOrphanFixities _ = return ()

  goInstance :: Declaration -> Ident -> [Constraint] -> Qualified ProperName -> [Type] -> Check Declaration
  goInstance d dictName deps className tys = do
    mapM_ (checkTypeClassInstance moduleName) tys
    forM_ deps $ mapM_ (checkTypeClassInstance moduleName) . snd
    checkOrphanInstance moduleName className tys
    let dict = TypeClassDictionaryInScope (Qualified (Just moduleName) dictName) className tys (Just deps) TCDRegular isInstanceExported
    addTypeClassDictionaries (Just moduleName) . M.singleton className $ M.singleton (canonicalizeDictionary dict) dict
    return d

    where

    checkOrphanInstance :: ModuleName -> Qualified ProperName -> [Type] -> Check ()
    checkOrphanInstance mn (Qualified (Just mn') _) tys
      | mn == mn' || any checkType tys = return ()
      | otherwise = throwError . errorMessage $ OrphanInstance dictName className tys
      where
      checkType :: Type -> Bool
      checkType (TypeVar _) = False
      checkType (TypeConstructor (Qualified (Just mn'') _)) = mn == mn''
      checkType (TypeConstructor (Qualified Nothing _)) = error "Unqualified type name in checkOrphanInstance"
      checkType (TypeApp t1 _) = checkType t1
      checkType _ = error "Invalid type in instance in checkOrphanInstance"
    checkOrphanInstance _ _ _ = error "Unqualified class name in checkOrphanInstance"

    isInstanceExported :: Bool
    isInstanceExported = any exportsInstance exps

    exportsInstance :: DeclarationRef -> Bool
    exportsInstance (TypeInstanceRef name) | name == dictName = True
    exportsInstance (PositionedDeclarationRef _ _ r) = exportsInstance r
    exportsInstance _ = False

  -- |
  -- This function adds the argument kinds for a type constructor so that they may appear in the externs file,
  -- extracted from the kind of the type constructor itself.
  --
  withKinds :: [(String, Maybe Kind)] -> Kind -> [(String, Maybe Kind)]
  withKinds []                  _               = []
  withKinds (s@(_, Just _ ):ss) (FunKind _   k) = s : withKinds ss k
  withKinds (  (s, Nothing):ss) (FunKind k1 k2) = (s, Just k1) : withKinds ss k2
  withKinds _                   _               = error "Invalid arguments to peelKinds"

-- |
-- Type check an entire module and ensure all types and classes defined within the module that are
-- required by exported members are also exported.
--
typeCheckModule :: Maybe ModuleName -> Module -> Check Module
typeCheckModule _ (Module _ _ _ _ Nothing) = error "exports should have been elaborated"
typeCheckModule mainModuleName (Module ss coms mn decls (Just exps)) = rethrow (onErrorMessages (ErrorInModule mn)) $ do
  modify (\s -> s { checkCurrentModule = Just mn })
  decls' <- typeCheckAll mainModuleName mn exps decls
  forM_ exps $ \e -> do
    checkTypesAreExported e
    checkClassMembersAreExported e
    checkClassesAreExported e
  return $ Module ss coms mn decls' (Just exps)
  where

  checkMemberExport :: (Type -> [DeclarationRef]) -> DeclarationRef -> Check ()
  checkMemberExport extract dr@(ValueRef name) = do
    ty <- lookupVariable mn (Qualified (Just mn) name)
    case filter (not . exported) (extract ty) of
      [] -> return ()
      hidden -> throwError . errorMessage $ TransitiveExportError dr hidden
      where
      exported e = any (exports e) exps
      exports (TypeRef pn1 _) (TypeRef pn2 _) = pn1 == pn2
      exports (ValueRef id1) (ValueRef id2) = id1 == id2
      exports (TypeClassRef pn1) (TypeClassRef pn2) = pn1 == pn2
      exports (TypeInstanceRef id1) (TypeInstanceRef id2) = id1 == id2
      exports (PositionedDeclarationRef _ _ r1) r2 = exports r1 r2
      exports r1 (PositionedDeclarationRef _ _ r2) = exports r1 r2
      exports _ _ = False
  checkMemberExport _ _ = return ()

  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> Check ()
  checkTypesAreExported = checkMemberExport findTcons
    where
    findTcons :: Type -> [DeclarationRef]
    findTcons = everythingOnTypes (++) go
      where
      go (TypeConstructor (Qualified (Just mn') name)) | mn' == mn = [TypeRef name (error "Data constructors unused in checkTypesAreExported")]
      go _ = []

  -- Check that all the classes defined in the current module that appear in member types have also
  -- been exported from the module
  checkClassesAreExported :: DeclarationRef -> Check ()
  checkClassesAreExported = checkMemberExport findClasses
    where
    findClasses :: Type -> [DeclarationRef]
    findClasses = everythingOnTypes (++) go
      where
      go (ConstrainedType cs _) = mapMaybe (fmap TypeClassRef . extractCurrentModuleClass . fst) cs
      go _ = []
    extractCurrentModuleClass :: Qualified ProperName -> Maybe ProperName
    extractCurrentModuleClass (Qualified (Just mn') name) | mn == mn' = Just name
    extractCurrentModuleClass _ = Nothing

  checkClassMembersAreExported :: DeclarationRef -> Check ()
  checkClassMembersAreExported dr@(TypeClassRef name) = do
    let members = ValueRef `map` head (mapMaybe findClassMembers decls)
    let missingMembers = members \\ exps
    unless (null missingMembers) $ throwError . errorMessage $ TransitiveExportError dr members
    where
    findClassMembers :: Declaration -> Maybe [Ident]
    findClassMembers (TypeClassDeclaration name' _ _ ds) | name == name' = Just $ map extractMemberName ds
    findClassMembers (PositionedDeclaration _ _ d) = findClassMembers d
    findClassMembers _ = Nothing
    extractMemberName :: Declaration -> Ident
    extractMemberName (PositionedDeclaration _ _ d) = extractMemberName d
    extractMemberName (TypeDeclaration memberName _) = memberName
    extractMemberName _ = error "Unexpected declaration in typeclass member list"
  checkClassMembersAreExported _ = return ()
