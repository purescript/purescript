{-# LANGUAGE FlexibleInstances #-}

-- |
-- The top-level type checker, which checks all declarations in a module.
--
module Language.PureScript.TypeChecker
  ( module T
  , typeCheckModule
  , checkNewtype
  ) where

import Prelude.Compat
import Protolude (headMay, ordNub)

import Control.Monad (when, unless, void, forM,)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), modify, gets)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter(..), censor)

import Data.Foldable (for_, traverse_, toList)
import Data.List (nub, nubBy, (\\), sort, group, intersect)
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Language.PureScript.AST
import qualified Language.PureScript.Constants.Data.Generic.Rep as DataGenericRep
import qualified Language.PureScript.Constants.Data.Newtype as DataNewtype
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Linter
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.Sugar.Names.Env (Exports(..))
import Language.PureScript.TypeChecker.Kinds as T
import Language.PureScript.TypeChecker.Monad as T
import Language.PureScript.TypeChecker.Roles as T
import Language.PureScript.TypeChecker.Synonyms as T
import Language.PureScript.TypeChecker.Types as T
import Language.PureScript.TypeChecker.Unify (varIfUnknown)
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import Lens.Micro.Platform ((^..), _2)

addDataType
  :: (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> DataDeclType
  -> ProperName 'TypeName
  -> [(Text, Maybe SourceType, Role)]
  -> [(DataConstructorDeclaration, SourceType)]
  -> SourceType
  -> m ()
addDataType moduleName dtype name args dctors ctorKind = do
  env <- getEnv
  let mapDataCtor (DataConstructorDeclaration _ ctorName vars) = (ctorName, snd <$> vars)
      qualName = (Qualified (Just moduleName) name)
      hasSig = qualName `M.member` types env
  putEnv $ env { types = M.insert qualName (ctorKind, DataType dtype args (map (mapDataCtor . fst) dctors)) (types env) }
  unless (hasSig || not (containsForAll ctorKind)) $ do
    tell . errorMessage $ MissingKindDeclaration (if dtype == Newtype then NewtypeSig else DataSig) name ctorKind
  for_ dctors $ \(DataConstructorDeclaration _ dctor fields, polyType) ->
    warnAndRethrow (addHint (ErrorInDataConstructor dctor)) $
      addDataConstructor moduleName dtype name dctor fields polyType

addDataConstructor
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => ModuleName
  -> DataDeclType
  -> ProperName 'TypeName
  -> ProperName 'ConstructorName
  -> [(Ident, SourceType)]
  -> SourceType
  -> m ()
addDataConstructor moduleName dtype name dctor dctorArgs polyType = do
  let fields = fst <$> dctorArgs
  env <- getEnv
  checkTypeSynonyms polyType
  putEnv $ env { dataConstructors = M.insert (Qualified (Just moduleName) dctor) (dtype, name, polyType, fields) (dataConstructors env) }

-- | Add an explicit role declaration to the Environment. The idea is that we
-- do this before encountering the data type which it refers to; we don't check
-- that the role declaration is valid until we encounter the data type's own
-- declaration.
addExplicitRoleDeclaration
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => ModuleName
  -> ProperName 'TypeName
  -> [Role]
  -> m ()
addExplicitRoleDeclaration moduleName name roles = do
  env <- getEnv
  putEnv $ env { roleDeclarations = M.insert (Qualified (Just moduleName) name) roles (roleDeclarations env) }

addTypeSynonym
  :: (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> ProperName 'TypeName
  -> [(Text, Maybe SourceType)]
  -> SourceType
  -> SourceType
  -> m ()
addTypeSynonym moduleName name args ty kind = do
  env <- getEnv
  checkTypeSynonyms ty
  let qualName = (Qualified (Just moduleName) name)
      hasSig = qualName `M.member` types env
  unless (hasSig || isDictSynonym name || not (containsForAll kind)) $ do
    tell . errorMessage $ MissingKindDeclaration TypeSynonymSig name kind
  putEnv $ env { types = M.insert qualName (kind, TypeSynonym) (types env)
               , typeSynonyms = M.insert qualName (args, ty) (typeSynonyms env) }

valueIsNotDefined
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => ModuleName
  -> Ident
  -> m ()
valueIsNotDefined moduleName name = do
  env <- getEnv
  case M.lookup (Qualified (Just moduleName) name) (names env) of
    Just _ -> throwError . errorMessage $ RedefinedIdent name
    Nothing -> return ()

addValue
  :: (MonadState CheckState m)
  => ModuleName
  -> Ident
  -> SourceType
  -> NameKind
  -> m ()
addValue moduleName name ty nameKind = do
  env <- getEnv
  putEnv (env { names = M.insert (Qualified (Just moduleName) name) (ty, nameKind, Defined) (names env) })

addTypeClass
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> Qualified (ProperName 'ClassName)
  -> [(Text, Maybe SourceType)]
  -> [SourceConstraint]
  -> [FunctionalDependency]
  -> [Declaration]
  -> SourceType
  -> m ()
addTypeClass _ qualifiedClassName args implies dependencies ds kind = do
  env <- getEnv
  newClass <- mkNewClass
  let qualName = fmap coerceProperName qualifiedClassName
      hasSig = qualName `M.member` types env
  unless (hasSig || not (containsForAll kind)) $ do
    tell . errorMessage $ MissingKindDeclaration ClassSig (disqualify qualName) kind
  traverse_ (checkMemberIsUsable newClass (typeSynonyms env) (types env)) classMembers
  putEnv $ env { types = M.insert qualName (kind, ExternData (nominalRolesForKind kind)) (types env)
               , typeClasses = M.insert qualifiedClassName newClass (typeClasses env) }
  where
    classMembers :: [(Ident, SourceType)]
    classMembers = map toPair ds

    mkNewClass :: m TypeClassData
    mkNewClass = do
      env <- getEnv
      implies' <- (traverse . overConstraintArgs . traverse) replaceAllTypeSynonyms implies
      let ctIsEmpty = null classMembers && all (typeClassIsEmpty . findSuperClass env) implies'
      pure $ makeTypeClassData args classMembers implies' dependencies ctIsEmpty
      where
      findSuperClass env c = case M.lookup (constraintClass c) (typeClasses env) of
        Just tcd -> tcd
        Nothing -> internalError "Unknown super class in TypeClassDeclaration"

    coveringSets :: TypeClassData -> [S.Set Int]
    coveringSets = S.toList . typeClassCoveringSets

    argToIndex :: Text -> Maybe Int
    argToIndex = flip M.lookup $ M.fromList (zipWith ((,) . fst) args [0..])

    toPair (TypeDeclaration (TypeDeclarationData _ ident ty)) = (ident, ty)
    toPair _ = internalError "Invalid declaration in TypeClassDeclaration"

    -- Currently we are only checking usability based on the type class currently
    -- being defined.  If the mentioned arguments don't include a covering set,
    -- then we won't be able to find a instance.
    checkMemberIsUsable :: TypeClassData -> T.SynonymMap -> T.KindMap -> (Ident, SourceType) -> m ()
    checkMemberIsUsable newClass syns kinds (ident, memberTy) = do
      memberTy' <- T.replaceAllTypeSynonymsM syns kinds memberTy
      let mentionedArgIndexes = S.fromList (mapMaybe argToIndex (freeTypeVariables memberTy'))
      let leftovers = map (`S.difference` mentionedArgIndexes) (coveringSets newClass)

      unless (any null leftovers) . throwError . errorMessage $
        let
          solutions = map (map (fst . (args !!)) . S.toList) leftovers
        in
          UnusableDeclaration ident (nub solutions)

addTypeClassDictionaries
  :: (MonadState CheckState m)
  => Maybe ModuleName
  -> M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict))
  -> m ()
addTypeClassDictionaries mn entries =
  modify $ \st -> st { checkEnv = (checkEnv st) { typeClassDictionaries = insertState st } }
  where insertState st = M.insertWith (M.unionWith (M.unionWith (<>))) mn entries (typeClassDictionaries . checkEnv $ st)

checkDuplicateTypeArguments
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => [Text]
  -> m ()
checkDuplicateTypeArguments args = for_ firstDup $ \dup ->
  throwError . errorMessage $ DuplicateTypeArgument dup
  where
  firstDup :: Maybe Text
  firstDup = listToMaybe $ args \\ ordNub args

checkTypeClassInstance
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => TypeClassData
  -> Int -- ^ index of type class argument
  -> SourceType
  -> m ()
checkTypeClassInstance cls i = check where
  -- If the argument is determined via fundeps then we are less restrictive in
  -- what type is allowed. This is because the type cannot be used to influence
  -- which instance is selected. Currently the only weakened restriction is that
  -- row types are allowed in determined type class arguments.
  isFunDepDetermined = S.member i (typeClassDeterminedArguments cls)
  check = \case
    TypeVar _ _ -> return ()
    TypeLevelString _ _ -> return ()
    TypeConstructor _ _ -> return ()
    TypeApp _ t1 t2 -> check t1 >> check t2
    KindApp _ t k -> check t >> check k
    KindedType _ t _ -> check t
    REmpty _ | isFunDepDetermined -> return ()
    RCons _ _ hd tl | isFunDepDetermined -> check hd >> check tl
    ty -> throwError . errorMessage $ InvalidInstanceHead ty

-- |
-- Check that type synonyms are fully-applied in a type
--
checkTypeSynonyms
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => SourceType
  -> m ()
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
--  * Infer all type roles and add them to the @Environment@
--
--  * Bring type class instances into scope
--
--  * Process module imports
--
typeCheckAll
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> [DeclarationRef]
  -> [Declaration]
  -> m [Declaration]
typeCheckAll moduleName _ = traverse go
  where
  go :: Declaration -> m Declaration
  go (DataDeclaration sa@(ss, _) dtype name args dctors) = do
    warnAndRethrow (addHint (ErrorInTypeConstructor name) . addHint (positionedError ss)) $ do
      when (dtype == Newtype) $ checkNewtype name dctors
      checkDuplicateTypeArguments $ map fst args
      (dataCtors, ctorKind) <- kindOfData moduleName (sa, name, args, dctors)
      let args' = args `withKinds` ctorKind
      env <- getEnv
      dctors' <- traverse (replaceTypeSynonymsInDataConstructor . fst) dataCtors
      roles <- checkRoles env moduleName name args' dctors'
      let args'' = args' `withRoles` roles
      addDataType moduleName dtype name args'' dataCtors ctorKind
    return $ DataDeclaration sa dtype name args dctors
  go (d@(DataBindingGroupDeclaration tys)) = do
    let tysList = NEL.toList tys
        syns = mapMaybe toTypeSynonym tysList
        dataDecls = mapMaybe toDataDecl tysList
        clss = mapMaybe toClassDecl tysList
        bindingGroupNames = ordNub ((syns^..traverse._2) ++ (dataDecls^..traverse._2._2) ++ (fmap coerceProperName (clss^..traverse._2._2)))
        sss = fmap declSourceSpan tys
    warnAndRethrow (addHint (ErrorInDataBindingGroup bindingGroupNames) . addHint (PositionedError sss)) $ do
      env <- getEnv
      (syn_ks, data_ks, cls_ks) <- kindsOfAll moduleName syns (fmap snd dataDecls) (fmap snd clss)
      for_ (zip syns syn_ks) $ \((_, name, args, _), (elabTy, kind)) -> do
        checkDuplicateTypeArguments $ map fst args
        let args' = args `withKinds` kind
        addTypeSynonym moduleName name args' elabTy kind
      let dataDeclsWithKinds = zipWith (\(dtype, (_, name, args, _)) (dataCtors, ctorKind) ->
            (dtype, name, args `withKinds` ctorKind, dataCtors, ctorKind)) dataDecls data_ks
      checkRoles' <- fmap (checkDataBindingGroupRoles env moduleName) .
        forM dataDeclsWithKinds $ \(_, name, args, dataCtors, _) ->
          (name, args,) <$> traverse (replaceTypeSynonymsInDataConstructor . fst) dataCtors
      for_ dataDeclsWithKinds $ \(dtype, name, args', dataCtors, ctorKind) -> do
        when (dtype == Newtype) $ checkNewtype name (map fst dataCtors)
        checkDuplicateTypeArguments $ map fst args'
        roles <- checkRoles' name args'
        let args'' = args' `withRoles` roles
        addDataType moduleName dtype name args'' dataCtors ctorKind
      for_ (zip clss cls_ks) $ \((deps, (sa, pn, _, _, _)), (args', implies', tys', kind)) -> do
        let qualifiedClassName = Qualified (Just moduleName) pn
        guardWith (errorMessage (DuplicateTypeClass pn (fst sa))) $
          not (M.member qualifiedClassName (typeClasses env))
        addTypeClass moduleName qualifiedClassName (fmap Just <$> args') implies' deps tys' kind
    return d
    where
    toTypeSynonym (TypeSynonymDeclaration sa nm args ty) = Just (sa, nm, args, ty)
    toTypeSynonym _ = Nothing
    toDataDecl (DataDeclaration sa dtype nm args dctors) = Just (dtype, (sa, nm, args, dctors))
    toDataDecl _ = Nothing
    toClassDecl (TypeClassDeclaration sa nm args implies deps decls) = Just (deps, (sa, nm, args, implies, decls))
    toClassDecl _ = Nothing
  go (TypeSynonymDeclaration sa@(ss, _) name args ty) = do
    warnAndRethrow (addHint (ErrorInTypeSynonym name) . addHint (positionedError ss) ) $ do
      checkDuplicateTypeArguments $ map fst args
      (elabTy, kind) <- kindOfTypeSynonym moduleName (sa, name, args, ty)
      let args' = args `withKinds` kind
      addTypeSynonym moduleName name args' elabTy kind
    return $ TypeSynonymDeclaration sa name args ty
  go (KindDeclaration sa@(ss, _) kindFor name ty) = do
    warnAndRethrow (addHint (ErrorInKindDeclaration name) . addHint (positionedError ss)) $ do
      elabTy <- withFreshSubstitution $ checkKindDeclaration moduleName ty
      env <- getEnv
      putEnv $ env { types = M.insert (Qualified (Just moduleName) name) (elabTy, LocalTypeVariable) (types env) }
      return $ KindDeclaration sa kindFor name elabTy
  go d@(RoleDeclaration (RoleDeclarationData _sa name roles)) = do
    addExplicitRoleDeclaration moduleName name roles
    return d
  go TypeDeclaration{} =
    internalError "Type declarations should have been removed before typeCheckAlld"
  go (ValueDecl sa@(ss, _) name nameKind [] [MkUnguarded val]) = do
    env <- getEnv
    warnAndRethrow (addHint (ErrorInValueDeclaration name) . addHint (positionedError ss)) . censorLocalUnnamedWildcards val $ do
      val' <- checkExhaustiveExpr ss env moduleName val
      valueIsNotDefined moduleName name
      typesOf NonRecursiveBindingGroup moduleName [((sa, name), val')] >>= \case
        [(_, (val'', ty))] -> do
          addValue moduleName name ty nameKind
          return $ ValueDecl sa name nameKind [] [MkUnguarded val'']
        _ -> internalError "typesOf did not return a singleton"
  go ValueDeclaration{} = internalError "Binders were not desugared"
  go BoundValueDeclaration{} = internalError "BoundValueDeclaration should be desugared"
  go (BindingGroupDeclaration vals) = do
    env <- getEnv
    let sss = fmap (\(((ss, _), _), _, _) -> ss) vals
    warnAndRethrow (addHint (ErrorInBindingGroup (fmap (\((_, ident), _, _) -> ident) vals)) . addHint (PositionedError sss)) $ do
      for_ vals $ \((_, ident), _, _) -> valueIsNotDefined moduleName ident
      vals' <- NEL.toList <$> traverse (\(sai@((ss, _), _), nk, expr) -> (sai, nk,) <$> checkExhaustiveExpr ss env moduleName expr) vals
      tys <- typesOf RecursiveBindingGroup moduleName $ fmap (\(sai, _, ty) -> (sai, ty)) vals'
      vals'' <- forM [ (sai, val, nameKind, ty)
                     | (sai@(_, name), nameKind, _) <- vals'
                     , ((_, name'), (val, ty)) <- tys
                     , name == name'
                     ] $ \(sai@(_, name), val, nameKind, ty) -> do
        addValue moduleName name ty nameKind
        return (sai, nameKind, val)
      return . BindingGroupDeclaration $ NEL.fromList vals''
  go (d@(ExternDataDeclaration _ name kind)) = do
    elabKind <- withFreshSubstitution $ checkKindDeclaration moduleName kind
    env <- getEnv
    let qualName = Qualified (Just moduleName) name
    -- If there's an explicit role declaration, just trust it
    let roles = fromMaybe (nominalRolesForKind elabKind) $ M.lookup qualName (roleDeclarations env)
    putEnv $ env { types = M.insert qualName (elabKind, ExternData roles) (types env) }
    return d
  go (d@(ExternDeclaration (ss, _) name ty)) = do
    warnAndRethrow (addHint (ErrorInForeignImport name) . addHint (positionedError ss)) $ do
      env <- getEnv
      (elabTy, kind) <- withFreshSubstitution $ do
        ((unks, ty'), kind) <- kindOfWithUnknowns ty
        pure (varIfUnknown unks ty', kind)
      checkTypeKind elabTy kind
      case M.lookup (Qualified (Just moduleName) name) (names env) of
        Just _ -> throwError . errorMessage $ RedefinedIdent name
        Nothing -> putEnv (env { names = M.insert (Qualified (Just moduleName) name) (elabTy, External, Defined) (names env) })
    return d
  go d@FixityDeclaration{} = return d
  go d@ImportDeclaration{} = return d
  go d@(TypeClassDeclaration sa@(ss, _) pn args implies deps tys) = do
    warnAndRethrow (addHint (ErrorInTypeClassDeclaration pn) . addHint (positionedError ss)) $ do
      env <- getEnv
      let qualifiedClassName = Qualified (Just moduleName) pn
      guardWith (errorMessage (DuplicateTypeClass pn ss)) $
        not (M.member qualifiedClassName (typeClasses env))
      (args', implies', tys', kind) <- kindOfClass moduleName (sa, pn, args, implies, tys)
      addTypeClass moduleName qualifiedClassName (fmap Just <$> args') implies' deps tys' kind
      return d
  go (d@(TypeInstanceDeclaration sa@(ss, _) ch idx dictName deps className tys body)) =
    rethrow (addHint (ErrorInInstance className tys) . addHint (positionedError ss)) $ do
      env <- getEnv
      let qualifiedDictName = Qualified (Just moduleName) dictName
      flip (traverse_ . traverse_) (typeClassDictionaries env) $ \dictionaries ->
        guardWith (errorMessage (DuplicateInstance dictName ss)) $
          not (M.member qualifiedDictName dictionaries)
      case M.lookup className (typeClasses env) of
        Nothing -> internalError "typeCheckAll: Encountered unknown type class in instance declaration"
        Just typeClass -> do
          checkInstanceArity dictName className typeClass tys
          (deps', kinds', tys', vars) <- withFreshSubstitution $ checkInstanceDeclaration moduleName (sa, deps, className, tys)
          tys'' <- traverse replaceAllTypeSynonyms tys'
          sequence_ (zipWith (checkTypeClassInstance typeClass) [0..] tys'')
          let nonOrphanModules = findNonOrphanModules className typeClass tys''
          checkOrphanInstance dictName className tys'' nonOrphanModules
          let qualifiedChain = Qualified (Just moduleName) <$> ch
          checkOverlappingInstance qualifiedChain dictName className typeClass tys'' nonOrphanModules
          _ <- traverseTypeInstanceBody checkInstanceMembers body
          deps'' <- (traverse . overConstraintArgs . traverse) replaceAllTypeSynonyms deps'
          let dict = TypeClassDictionaryInScope qualifiedChain idx qualifiedDictName [] className vars kinds' tys'' (Just deps'')
          addTypeClassDictionaries (Just moduleName) . M.singleton className $ M.singleton (tcdValue dict) (pure dict)
          return d

  checkInstanceArity :: Ident -> Qualified (ProperName 'ClassName) -> TypeClassData -> [SourceType] -> m ()
  checkInstanceArity dictName className typeClass tys = do
    let typeClassArity = length (typeClassArguments typeClass)
        instanceArity = length tys
    when (typeClassArity /= instanceArity) $
      throwError . errorMessage $ ClassInstanceArityMismatch dictName className typeClassArity instanceArity

  checkInstanceMembers :: [Declaration] -> m [Declaration]
  checkInstanceMembers instDecls = do
    let idents = sort . map head . group . map memberName $ instDecls
    for_ (firstDuplicate idents) $ \ident ->
      throwError . errorMessage $ DuplicateValueDeclaration ident
    return instDecls
    where
    memberName :: Declaration -> Ident
    memberName (ValueDeclaration vd) = valdeclIdent vd
    memberName _ = internalError "checkInstanceMembers: Invalid declaration in type instance definition"

    firstDuplicate :: (Eq a) => [a] -> Maybe a
    firstDuplicate (x : xs@(y : _))
      | x == y = Just x
      | otherwise = firstDuplicate xs
    firstDuplicate _ = Nothing

  findNonOrphanModules
    :: Qualified (ProperName 'ClassName)
    -> TypeClassData
    -> [SourceType]
    -> S.Set ModuleName
  findNonOrphanModules (Qualified (Just mn') _) typeClass tys' = nonOrphanModules
    where
    nonOrphanModules :: S.Set ModuleName
    nonOrphanModules = S.insert mn' nonOrphanModules'

    typeModule :: SourceType -> Maybe ModuleName
    typeModule (TypeVar _ _) = Nothing
    typeModule (TypeLevelString _ _) = Nothing
    typeModule (TypeConstructor _ (Qualified (Just mn'') _)) = Just mn''
    typeModule (TypeConstructor _ (Qualified Nothing _)) = internalError "Unqualified type name in findNonOrphanModules"
    typeModule (TypeApp _ t1 _) = typeModule t1
    typeModule (KindApp _ t1 _) = typeModule t1
    typeModule (KindedType _ t1 _) = typeModule t1
    typeModule _ = internalError "Invalid type in instance in findNonOrphanModules"

    modulesByTypeIndex :: M.Map Int (Maybe ModuleName)
    modulesByTypeIndex = M.fromList (zip [0 ..] (typeModule <$> tys'))

    lookupModule :: Int -> S.Set ModuleName
    lookupModule idx = case M.lookup idx modulesByTypeIndex of
      Just ms -> S.fromList (toList ms)
      Nothing -> internalError "Unknown type index in findNonOrphanModules"

    -- If the instance is declared in a module that wouldn't be found based on a covering set
    -- then it is considered an orphan - because we'd have a situation in which we expect an
    -- instance but can't find it. So a valid module must be applicable across *all* covering
    -- sets - therefore we take the intersection of covering set modules.
    nonOrphanModules' :: S.Set ModuleName
    nonOrphanModules' = foldl1 S.intersection (foldMap lookupModule `S.map` typeClassCoveringSets typeClass)
  findNonOrphanModules _ _ _ = internalError "Unqualified class name in findNonOrphanModules"

  -- Check that the instance currently being declared doesn't overlap with any
  -- other instance in any module that this instance wouldn't be considered an
  -- orphan in.  There are overlapping instance situations that won't be caught
  -- by this, for example when combining multiparameter type classes with
  -- flexible instances: the instances `Cls X y` and `Cls x Y` overlap and
  -- could live in different modules but won't be caught here.
  checkOverlappingInstance
    :: [Qualified Ident]
    -> Ident
    -> Qualified (ProperName 'ClassName)
    -> TypeClassData
    -> [SourceType]
    -> S.Set ModuleName
    -> m ()
  checkOverlappingInstance ch dictName className typeClass tys' nonOrphanModules = do
    for_ nonOrphanModules $ \m -> do
      dicts <- M.toList <$> lookupTypeClassDictionariesForClass (Just m) className

      for_ dicts $ \(ident, dictNel) -> do
        for_ dictNel $ \dict -> do
          -- ignore instances in the same instance chain
          if ch == tcdChain dict ||
            instancesAreApart (typeClassCoveringSets typeClass) tys' (tcdInstanceTypes dict)
          then return ()
          else throwError . errorMessage $
                OverlappingInstances className
                                      tys'
                                      [ident, Qualified (Just moduleName) dictName]

  instancesAreApart
    :: S.Set (S.Set Int)
    -> [SourceType]
    -> [SourceType]
    -> Bool
  instancesAreApart sets lhs rhs = all (any typesApart . S.toList) (S.toList sets)
    where
      typesApart :: Int -> Bool
      typesApart i = typeHeadsApart (lhs !! i) (rhs !! i)

      -- Note: implementation doesn't need to care about all possible cases:
      -- TUnknown, Skolem, etc.
      typeHeadsApart :: SourceType -> SourceType -> Bool
      typeHeadsApart l                   r             | eqType l r = False
      typeHeadsApart (TypeVar _ _)       _                          = False
      typeHeadsApart _                   (TypeVar _ _)              = False
      typeHeadsApart (KindedType _ t1 _) t2                         = typeHeadsApart t1 t2
      typeHeadsApart t1                  (KindedType _ t2 _)        = typeHeadsApart t1 t2
      typeHeadsApart (TypeApp _ h1 t1)   (TypeApp _ h2 t2)          = typeHeadsApart h1 h2 || typeHeadsApart t1 t2
      typeHeadsApart _                   _                          = True

  checkOrphanInstance
    :: Ident
    -> Qualified (ProperName 'ClassName)
    -> [SourceType]
    -> S.Set ModuleName
    -> m ()
  checkOrphanInstance dictName className tys' nonOrphanModules
    | moduleName `S.member` nonOrphanModules = return ()
    | otherwise = throwError . errorMessage $ OrphanInstance dictName className nonOrphanModules tys'

  censorLocalUnnamedWildcards :: Expr -> m a -> m a
  censorLocalUnnamedWildcards (TypedValue _ _ ty) = censor (filterErrors (not . isLocalUnnamedWildcardError ty))
  censorLocalUnnamedWildcards _ = id

  isLocalUnnamedWildcardError :: SourceType -> ErrorMessage -> Bool
  isLocalUnnamedWildcardError ty err@(ErrorMessage _ (WildcardInferredType _ _)) =
    let
      ssWildcard (TypeWildcard (ss', _) Nothing) = [ss']
      ssWildcard _ = []
      sssWildcards = everythingOnTypes (<>) ssWildcard ty
      sss = maybe [] NEL.toList $ errorSpan err
    in
      null $ intersect sss sssWildcards
  isLocalUnnamedWildcardError _ _ = False

  -- |
  -- This function adds the argument kinds for a type constructor so that they may appear in the externs file,
  -- extracted from the kind of the type constructor itself.
  --
  withKinds :: [(Text, Maybe SourceType)] -> SourceType -> [(Text, Maybe SourceType)]
  withKinds [] _ = []
  withKinds ss (ForAll _ _ _ k _) = withKinds ss k
  withKinds (s@(_, Just _):ss) (TypeApp _ (TypeApp _ tyFn _) k2) | eqType tyFn tyFunction = s : withKinds ss k2
  withKinds ((s, Nothing):ss) (TypeApp _ (TypeApp _ tyFn k1) k2) | eqType tyFn tyFunction = (s, Just k1) : withKinds ss k2
  withKinds _ _ = internalError "Invalid arguments to withKinds"

  withRoles :: [(Text, Maybe SourceType)] -> [Role] -> [(Text, Maybe SourceType, Role)]
  withRoles = zipWith $ \(v, k) r -> (v, k, r)

  replaceTypeSynonymsInDataConstructor :: DataConstructorDeclaration -> m DataConstructorDeclaration
  replaceTypeSynonymsInDataConstructor DataConstructorDeclaration{..} = do
    dataCtorFields' <- traverse (traverse replaceAllTypeSynonyms) dataCtorFields
    return DataConstructorDeclaration
      { dataCtorFields = dataCtorFields'
      , ..
      }

checkNewtype
  :: forall m
   . MonadError MultipleErrors m
  => ProperName 'TypeName
  -> [DataConstructorDeclaration]
  -> m ()
checkNewtype _ [(DataConstructorDeclaration _ _ [_])] = return ()
checkNewtype name _ = throwError . errorMessage $ InvalidNewtype name

-- |
-- Type check an entire module and ensure all types and classes defined within the module that are
-- required by exported members are also exported.
--
typeCheckModule
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => M.Map ModuleName Exports
  -> Module
  -> m Module
typeCheckModule _ (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before typeCheckModule"
typeCheckModule modulesExports (Module ss coms mn decls (Just exps)) =
  warnAndRethrow (addHint (ErrorInModule mn)) $ do
    let (decls', imports) = partitionEithers $ fromImportDecl <$> decls
    modify (\s -> s { checkCurrentModule = Just mn, checkCurrentModuleImports = imports })
    decls'' <- typeCheckAll mn exps decls'
    checkSuperClassesAreExported <- getSuperClassExportCheck
    for_ exps $ \e -> do
      checkTypesAreExported e
      checkClassMembersAreExported e
      checkClassesAreExported e
      checkSuperClassesAreExported e
      checkDataConstructorsAreExported e
    return $ Module ss coms mn (map toImportDecl imports ++ decls'') (Just exps)
  where

  fromImportDecl
    :: Declaration
    -> Either Declaration
              ( SourceAnn
              , ModuleName
              , ImportDeclarationType
              , Maybe ModuleName
              , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
              )
  fromImportDecl (ImportDeclaration sa moduleName importDeclarationType asModuleName) =
    Right (sa, moduleName, importDeclarationType, asModuleName, foldMap exportedTypes $ M.lookup moduleName modulesExports)
  fromImportDecl decl = Left decl

  toImportDecl
    :: ( SourceAnn
       , ModuleName
       , ImportDeclarationType
       , Maybe ModuleName
       , M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
       )
    -> Declaration
  toImportDecl (sa, moduleName, importDeclarationType, asModuleName, _) =
    ImportDeclaration sa moduleName importDeclarationType asModuleName

  qualify' :: a -> Qualified a
  qualify' = Qualified (Just mn)

  getSuperClassExportCheck = do
    classesToSuperClasses <- gets
      ( M.map
        ( S.fromList
        . filter (\(Qualified mn' _) -> mn' == Just mn)
        . fmap constraintClass
        . typeClassSuperclasses
        )
      . typeClasses
      . checkEnv
      )
    let
      -- A function that, given a class name, returns the set of
      -- transitive class dependencies that are defined in this
      -- module.
      transitiveSuperClassesFor
          :: Qualified (ProperName 'ClassName)
          -> S.Set (Qualified (ProperName 'ClassName))
      transitiveSuperClassesFor qname =
        untilSame
          (\s -> s <> foldMap (\n -> fromMaybe S.empty (M.lookup n classesToSuperClasses)) s)
          (fromMaybe S.empty (M.lookup qname classesToSuperClasses))

      superClassesFor qname =
        fromMaybe S.empty (M.lookup qname classesToSuperClasses)

    pure $ checkSuperClassExport superClassesFor transitiveSuperClassesFor
  moduleClassExports :: S.Set (Qualified (ProperName 'ClassName))
  moduleClassExports = S.fromList $ mapMaybe (\x -> case x of
     TypeClassRef _ name -> Just (qualify' name)
     _ -> Nothing) exps

  untilSame :: Eq a => (a -> a) -> a -> a
  untilSame f a = let a' = f a in if a == a' then a else untilSame f a'

  checkMemberExport :: (SourceType -> [DeclarationRef]) -> DeclarationRef -> m ()
  checkMemberExport extract dr@(TypeRef _ name dctors) = do
    env <- getEnv
    for_ (M.lookup (qualify' name) (types env)) $ \(k, _) -> do
      -- TODO: remove?
      -- let findModuleKinds = everythingOnTypes (++) $ \case
      --       TypeConstructor _ (Qualified (Just mn') kindName) | mn' == mn -> [kindName]
      --       _ -> []
      checkExport dr (extract k)
    for_ (M.lookup (qualify' name) (typeSynonyms env)) $ \(_, ty) ->
      checkExport dr (extract ty)
    for_ dctors $ \dctors' ->
      for_ dctors' $ \dctor ->
        for_ (M.lookup (qualify' dctor) (dataConstructors env)) $ \(_, _, ty, _) ->
          checkExport dr (extract ty)
  checkMemberExport extract dr@(ValueRef _ name) = do
    ty <- lookupVariable (qualify' name)
    checkExport dr (extract ty)
  checkMemberExport _ _ = return ()

  checkSuperClassExport
    :: (Qualified (ProperName 'ClassName) -> S.Set (Qualified (ProperName 'ClassName)))
    -> (Qualified (ProperName 'ClassName) -> S.Set (Qualified (ProperName 'ClassName)))
    -> DeclarationRef
    -> m ()
  checkSuperClassExport superClassesFor transitiveSuperClassesFor dr@(TypeClassRef drss className) = do
    let superClasses = superClassesFor (qualify' className)
        -- thanks to laziness, the computation of the transitive
        -- superclasses defined in-module will only occur if we actually
        -- throw the error. Constructing the full set of transitive
        -- superclasses is likely to be costly for every single term.
        transitiveSuperClasses = transitiveSuperClassesFor (qualify' className)
        unexported = S.difference superClasses moduleClassExports
    unless (null unexported)
      . throwError . errorMessage' drss
      . TransitiveExportError dr
      . map (TypeClassRef drss . disqualify)
      $ toList transitiveSuperClasses
  checkSuperClassExport _ _ _ =
    return ()

  checkExport :: DeclarationRef -> [DeclarationRef] -> m ()
  checkExport dr drs = case filter (not . exported) drs of
    [] -> return ()
    hidden -> throwError . errorMessage' (declRefSourceSpan dr) $ TransitiveExportError dr (nubBy nubEq hidden)
    where
    exported e = any (exports e) exps
    exports (TypeRef _ pn1 _) (TypeRef _ pn2 _) = pn1 == pn2
    exports (ValueRef _ id1) (ValueRef _ id2) = id1 == id2
    exports (TypeClassRef _ pn1) (TypeClassRef _ pn2) = pn1 == pn2
    exports _ _ = False
    -- We avoid Eq for `nub`bing as the dctor part of `TypeRef` evaluates to
    -- `error` for the values generated here (we don't need them anyway)
    nubEq (TypeRef _ pn1 _) (TypeRef _ pn2 _) = pn1 == pn2
    nubEq r1 r2 = r1 == r2


  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> m ()
  checkTypesAreExported ref = checkMemberExport findTcons ref
    where
    findTcons :: SourceType -> [DeclarationRef]
    findTcons = everythingOnTypes (++) go
      where
      go (TypeConstructor _ (Qualified (Just mn') name)) | mn' == mn =
        [TypeRef (declRefSourceSpan ref) name (internalError "Data constructors unused in checkTypesAreExported")]
      go _ = []

  -- Check that all the classes defined in the current module that appear in member types have also
  -- been exported from the module
  checkClassesAreExported :: DeclarationRef -> m ()
  checkClassesAreExported ref = checkMemberExport findClasses ref
    where
    findClasses :: SourceType -> [DeclarationRef]
    findClasses = everythingOnTypes (++) go
      where
      go (ConstrainedType _ c _) = (fmap (TypeClassRef (declRefSourceSpan ref)) . extractCurrentModuleClass . constraintClass) c
      go _ = []
    extractCurrentModuleClass :: Qualified (ProperName 'ClassName) -> [ProperName 'ClassName]
    extractCurrentModuleClass (Qualified (Just mn') name) | mn == mn' = [name]
    extractCurrentModuleClass _ = []

  checkClassMembersAreExported :: DeclarationRef -> m ()
  checkClassMembersAreExported dr@(TypeClassRef ss' name) = do
    let members = ValueRef ss' `map` head (mapMaybe findClassMembers decls)
    let missingMembers = members \\ exps
    unless (null missingMembers) . throwError . errorMessage' ss' $ TransitiveExportError dr missingMembers
    where
    findClassMembers :: Declaration -> Maybe [Ident]
    findClassMembers (TypeClassDeclaration _ name' _ _ _ ds) | name == name' = Just $ map extractMemberName ds
    findClassMembers (DataBindingGroupDeclaration decls') = headMay . mapMaybe findClassMembers $ NEL.toList decls'
    findClassMembers _ = Nothing
    extractMemberName :: Declaration -> Ident
    extractMemberName (TypeDeclaration td) = tydeclIdent td
    extractMemberName _ = internalError "Unexpected declaration in typeclass member list"
  checkClassMembersAreExported _ = return ()

  -- If a type is exported without data constructors, we warn on `Generic` or `Newtype` instances.
  -- On the other hand if any data constructors are exported, we require all of them to be exported.
  checkDataConstructorsAreExported :: DeclarationRef -> m ()
  checkDataConstructorsAreExported dr@(TypeRef ss' name (fromMaybe [] -> exportedDataConstructorsNames))
    | null exportedDataConstructorsNames = for_
      [ DataGenericRep.Generic
      , DataNewtype.Newtype
      ] $ \className -> do
        env <- getEnv
        let dicts = foldMap (foldMap NEL.toList) $
              M.lookup (Just mn) (typeClassDictionaries env) >>= M.lookup className
        when (any isDictOfTypeRef dicts) $
          tell . errorMessage' ss' $ HiddenConstructors dr className
    | otherwise = do
      env <- getEnv
      let dataConstructorNames = fromMaybe [] $
            M.lookup (mkQualified name mn) (types env) >>= getDataConstructorNames . snd
          missingDataConstructorsNames = dataConstructorNames \\ exportedDataConstructorsNames
      unless (null missingDataConstructorsNames) $
        throwError . errorMessage' ss' $ TransitiveDctorExportError dr missingDataConstructorsNames
      where
      isDictOfTypeRef :: TypeClassDictionaryInScope a -> Bool
      isDictOfTypeRef dict
        | (TypeConstructor _ qualTyName, _, _) : _ <- unapplyTypes <$> tcdInstanceTypes dict
        , qualTyName == Qualified (Just mn) name
        = True
      isDictOfTypeRef _ = False
      getDataConstructorNames :: TypeKind -> Maybe [ProperName 'ConstructorName]
      getDataConstructorNames (DataType _ _ constructors) = Just $ fst <$> constructors
      getDataConstructorNames _ = Nothing
  checkDataConstructorsAreExported _ = return ()
