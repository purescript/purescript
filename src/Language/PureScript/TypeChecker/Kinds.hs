{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- This module implements the kind checker
--
module Language.PureScript.TypeChecker.Kinds
  ( kindOf
  , kindOfWithScopedVars
  , kindOfData
  , kindOfTypeSynonym
  , kindOfClass
  , kindsOfAll
  , unifyKinds
  , checkKind
  , inferKind
  , checkInstanceDeclaration
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State

import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.IntSet as IS
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Language.PureScript.Crash
import qualified Language.PureScript.Environment as E
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types
import Language.PureScript.Pretty.Types
import Lens.Micro.Platform ((^.), _1, _2, _3)

generalizeUnknowns :: [(Unknown, SourceType)] -> SourceType -> SourceType
generalizeUnknowns unks ty =
  generalizeUnknownsWithVars (unknownVarsForType unks ty) ty

generalizeUnknownsWithVars :: [(Unknown, (Text, Maybe SourceType))] -> SourceType -> SourceType
generalizeUnknownsWithVars binders ty =
  mkForAll ((getAnnForType ty,) . snd <$> binders) . replaceUnknownsWithVars binders $ ty

replaceUnknownsWithVars :: [(Unknown, (Text, Maybe SourceType))] -> SourceType -> SourceType
replaceUnknownsWithVars binders ty
  | null binders = ty
  | otherwise = go ty
  where
  go :: SourceType -> SourceType
  go = everywhereOnTypes $ \case
    TUnknown ann unk | Just (name, _) <- lookup unk binders -> TypeVar ann name
    other -> other

unknownVarsForType :: [(Unknown, SourceType)] -> SourceType -> [(Unknown, (Text, Maybe SourceType))]
unknownVarsForType unks ty =
  fmap (\(a, b) -> (a, (genName a, Just b))) unks
  where
  inUse :: [Text]
  inUse = usedTypeVariables ty

  genName :: Int -> T.Text
  genName i = do
    let name = "k" <> T.pack (show i)
    if name `elem` inUse then genName (i + 1) else name

apply :: (MonadState CheckState m) => SourceType -> m SourceType
apply ty = flip substituteType ty <$> gets checkSubstitution

substituteType :: Substitution -> SourceType -> SourceType
substituteType sub = everywhereOnTypes $ \case
  TUnknown ann u ->
    case M.lookup u (substType sub) of
      Nothing -> TUnknown ann u
      Just (TUnknown ann' u1) | u1 == u -> TUnknown ann' u1
      Just t -> substituteType sub t
  other ->
    other

freshUnknown :: (MonadState CheckState m) => m Unknown
freshUnknown = do
  k <- gets checkNextType
  modify $ \st -> st { checkNextType = k + 1 }
  pure k

freshKind :: (MonadState CheckState m) => m SourceType
freshKind = freshKindWithKind E.kindType

freshKindWithKind :: (MonadState CheckState m) => SourceType -> m SourceType
freshKindWithKind kind = do
  u <- freshUnknown
  addUnsolved Nothing u kind
  pure $ TUnknown nullSourceAnn u

addUnsolved :: (MonadState CheckState m) => Maybe UnkLevel -> Unknown -> SourceType -> m ()
addUnsolved lvl unk kind = modify $ \st -> do
  let
    newLvl = UnkLevel $ case lvl of
      Nothing -> pure unk
      Just (UnkLevel lvl') -> lvl' <> pure unk
    subs = checkSubstitution st
    uns = M.insert unk (newLvl, kind) $ substUnsolved subs
  st { checkSubstitution = subs { substUnsolved = uns } }

solve :: (MonadState CheckState m) => Unknown -> SourceType -> m ()
solve unk solution = modify $ \st -> do
  let
    subs = checkSubstitution st
    tys = M.insert unk solution $ substType subs
  st { checkSubstitution = subs { substType = tys } }

lookupUnsolved
  :: (MonadState CheckState m, MonadError MultipleErrors m, HasCallStack)
  => Unknown
  -> m (UnkLevel, SourceType)
lookupUnsolved u = do
  uns <- gets (substUnsolved . checkSubstitution)
  case M.lookup u uns of
    Nothing -> internalError "Unsolved variable is not bound" -- TODO
    Just res -> return res

unknownsWithKinds
  :: forall m. (MonadState CheckState m, MonadError MultipleErrors m)
  => [Unknown]
  -> m [(Unknown, SourceType)]
unknownsWithKinds = fmap (fmap snd . sortBy (comparing fst) . join) . traverse go
  where
  go u = do
    (lvl, ty) <- traverse apply =<< lookupUnsolved u
    rest <- fmap join . traverse go . IS.toList . unknowns $ ty
    pure $ (lvl, (u, ty)) : rest

inferKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (SourceType, SourceType)
inferKind = \case
  ty@(TypeConstructor ann v) -> do
    env <- getEnv
    case M.lookup v (E.types env) of
      Nothing ->
        throwError . errorMessage' (fst ann) . UnknownName . fmap TyName $ v
      Just (kind, E.LocalTypeVariable) -> do
        kind' <- apply kind
        pure (ty, kind' $> ann)
      Just (kind, _) -> do
        pure (ty, kind $> ann)
  ConstrainedType _ (Constraint ann v args dat) ty -> do
    env <- getEnv
    args' <- case M.lookup (coerceProperName <$> v) (E.types env) of
      Nothing ->
        throwError . errorMessage' (fst ann) . UnknownName . fmap TyClassName $ v
      Just _ -> do
        let fakeTy = foldl (TypeApp ann) (TypeConstructor ann (coerceProperName <$> v)) args
        snd . unapplyTypes <$> checkKind fakeTy E.kindType
    ty' <- checkKind ty E.kindType
    pure (ConstrainedType ann (Constraint ann v args' dat) ty', E.kindType $> ann)
  ty@(TypeLevelString ann _) ->
    pure (ty, E.kindSymbol $> ann)
  ty@(TypeVar ann v) -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- apply =<< lookupTypeVariable moduleName (Qualified Nothing $ ProperName v)
    pure (ty, kind $> ann)
  ty@(Skolem ann v _ _) -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- apply =<< lookupTypeVariable moduleName (Qualified Nothing $ ProperName v)
    pure (ty, kind $> ann)
  ty@(TUnknown ann u) -> do
    (_, kind) <- lookupUnsolved u
    pure (ty, kind $> ann)
  ty@(TypeWildcard ann _) -> do
    -- TODO: Warning?
    k <- freshKind
    pure (ty, k $> ann)
  ty@(REmpty ann) -> do
    k <- freshKind
    pure (ty, E.kindRow k $> ann)
  ty@(RCons ann _ _ _) | (rowList, rempty@(REmpty _)) <- rowToList ty -> do
    kr <- freshKind
    rowList' <- for rowList $ \(RowListItem a lbl t) ->
      RowListItem a lbl <$> checkKind t kr
    kr' <- apply kr
    pure (rowFromList (rowList', rempty), E.kindRow kr' $> ann)
  TypeApp ann t1 t2 -> do
    (t1', k1) <- inferKind t1
    inferAppKind ann (t1', k1) t2
  KindApp ann t1 t2 -> do
    (t1', kind) <- bitraverse pure apply =<< inferKind t1
    case kind of
      ForAll _ arg (Just argKind) resKind _ -> do
        t2' <- checkKind t2 argKind
        pure (KindApp ann t1' t2', replaceTypeVars arg t2' resKind)
      _ ->
        -- TODO: Better error for bad KindApp
        internalError "inferKind: unkinded forall binder"
  KindedType _ t1 t2 -> do
    t1' <- checkKind t1 t2
    t2' <- apply t2
    pure (t1', t2')
  ForAll ann arg mbKind ty sc -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- case mbKind of
      Just k -> checkKind k E.kindType
      Nothing -> fmap ($> ann) freshKind
    (ty', unks) <- bindLocalTypeVariables moduleName [(ProperName arg, kind)] $ do
      ty' <- apply =<< checkKind ty E.kindType
      unks <- for (IS.toList $ unknowns ty') $ \u ->
        fmap (u,) . apply . snd =<< lookupUnsolved u
      pure (ty', unks)
    unless (not . elem arg $ foldMap (freeTypeVariables . snd) unks) $
      internalError "Quantification check failure" -- TODO
    for_ unks . uncurry $ addUnsolved Nothing
    pure (ForAll ann arg (Just kind) ty' sc, E.kindType $> ann)
  ty ->
    internalError $ "inferKind: Unimplemented case \n" <> prettyPrintType 100 ty <> show ty

inferAppKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceAnn
  -> (SourceType, SourceType)
  -> SourceType
  -> m (SourceType, SourceType)
inferAppKind ann (fn, fnKind) arg = case fnKind of
  TypeApp _ (TypeApp _ arrKind argKind) resKind
    | eqType arrKind E.tyFunction || eqType arrKind E.tyConstrainedValue -> do
        arg' <- checkKind arg argKind
        (TypeApp ann fn arg',) <$> apply resKind
  TUnknown _ u -> do
    (lvl, _) <- lookupUnsolved u
    u1 <- freshUnknown
    u2 <- freshUnknown
    addUnsolved (Just lvl) u1 E.kindType
    addUnsolved (Just lvl) u2 E.kindType
    solve u $ (TUnknown ann u1 E.-:> TUnknown ann u2) $> ann
    arg' <- checkKind arg $ TUnknown ann u1
    pure (TypeApp ann fn arg', TUnknown ann u2)
  ForAll _ a (Just k) ty _ -> do
    u <- freshUnknown
    addUnsolved Nothing u k
    inferAppKind ann (KindApp ann fn (TUnknown ann u), replaceTypeVars a (TUnknown ann u) ty) arg
  _ -> -- TODO
    internalError "Cannot apply type"

checkKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m SourceType
checkKind ty kind2 = do
  (ty', kind1) <- inferKind ty
  kind1' <- apply kind1
  kind2' <- apply kind2
  instantiateKind (ty', kind1') kind2'

instantiateKind
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => (SourceType, SourceType)
  -> SourceType
  -> m SourceType
instantiateKind (ty, kind1) kind2 = case kind1 of
  ForAll _ a (Just k) t _ -> do
    let ann = getAnnForType ty
    u <- freshKindWithKind k
    instantiateKind (KindApp ann ty u, replaceTypeVars a u t) kind2
  _ -> do
    unifyKinds kind1 kind2
    pure ty

unifyKinds
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> SourceType
  -> m ()
unifyKinds = curry $ \case
  (TypeApp _ p1 p2, TypeApp _ p3 p4) -> do
    unifyKinds p1 p3
    join $ unifyKinds <$> apply p2 <*> apply p4
  (KindApp _ p1 p2, KindApp _ p3 p4) -> do
    unifyKinds p1 p3
    join $ unifyKinds <$> apply p2 <*> apply p4
  (w1, w2) | eqType w1 w2 ->
    pure ()
  (TUnknown ss a', p1) ->
    unifyUnknown ss a' p1
  (p1, TUnknown ss a') ->
    unifyUnknown ss a' p1
  (w1, w2) ->
    throwError
      . errorMessage''' (fst . getAnnForType <$> [w1, w2])
      $ KindsDoNotUnify w1 w2
  where
  unifyUnknown _ a' p1 = do
    p2 <- promoteKind a' p1
    w1 <- snd <$> lookupUnsolved a'
    join $ unifyKinds <$> apply w1 <*> elaborateKind p2
    solve a' p2

promoteKind
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => Unknown
  -> SourceType
  -> m SourceType
promoteKind u2 ty = do
  lvl2 <- fst <$> lookupUnsolved u2
  flip everywhereOnTypesM ty $ \case
    -- TODO: Do we need to check free type variables?
    ty'@(TUnknown ann u1) -> do
      when (u1 == u2) . throwError . errorMessage . InfiniteKind $ ty
      (lvl1, k) <- lookupUnsolved u1
      if lvl1 < lvl2 then
        pure ty'
      else do
        k'  <- promoteKind u2 =<< apply k
        u1' <- freshUnknown
        addUnsolved (Just lvl2) u1' k'
        solve u1 $ TUnknown ann u1'
        pure $ TUnknown ann u1'
    ty' ->
      pure ty'

elaborateKind
  :: (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> m SourceType
elaborateKind = \case
  TypeLevelString ann _ ->
    pure $ E.kindType $> ann
  TypeConstructor ann v -> do
    env <- getEnv
    case M.lookup v (E.types env) of
      Nothing ->
        throwError . errorMessage' (fst ann) . UnknownName . fmap TyName $ v
      Just (kind, _) ->
        ($> ann) <$> apply kind
  TypeVar ann a -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- lookupTypeVariable moduleName . Qualified Nothing $ ProperName a
    ($> ann) <$> apply kind
  TUnknown ann a' -> do
    kind <- snd <$> lookupUnsolved a'
    ($> ann) <$> apply kind
  TypeApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      TypeApp _ (TypeApp _ k w1) w2 | eqType k E.tyFunction -> do
        k2 <- elaborateKind t2
        unless (eqType k2 w1) $ internalError "Cannot apply type" -- TODO
        pure $ w2 $> ann
      _ ->
        internalError "Cannot apply type" -- TODO
  KindApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      ForAll _ a (Just w) n _ -> do
        k2 <- elaborateKind t2
        unless (eqType k2 w) $ internalError "Cannot apply kind" -- TODO
        flip (replaceTypeVars a) n . ($> ann) <$> apply t2
      _ ->
        internalError "Cannot apply kind" -- TODO
  ForAll ann a (Just w) u _ -> do
    wIsStar <- elaborateKind w
    unless (eqType wIsStar E.kindType) $ internalError "Elaborated kind is not Type" -- TODO
    moduleName <- unsafeCheckCurrentModule
    uIsStar <- bindLocalTypeVariables moduleName [(ProperName a, w)] $ elaborateKind u
    unless (eqType uIsStar E.kindType) $ internalError "Elaborated kind is not Type" -- TODO
    pure $ E.kindType $> ann
  ty ->
    internalError "elaboratedKind: Unimplemented case" -- TODO

-- | Infer the kind of a single type
kindOf
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (SourceType, SourceType)
kindOf = fmap (first snd) . kindOfWithScopedVars

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType ->
  m (([(Text, SourceType)], SourceType), SourceType)
kindOfWithScopedVars ty = do
  (ty', kind) <- bindTypes mempty $ inferKind ty
  (binders, ty'') <- first sortBinderList . fromJust . completeBinderList <$> apply ty'
  let ty''' = foldr (\(ann, (ident, k)) t -> ForAll ann ident (Just k) t Nothing) ty'' binders
  pure ((fmap snd binders, ty'''), kind)

type DataDeclarationArgs =
  ( SourceAnn
  , ProperName 'TypeName
  , [(Text, Maybe SourceType)]
  , [DataConstructorDeclaration]
  )

type DataDeclarationResult =
  ( [(DataConstructorDeclaration, SourceType)]
  -- ^ The infered type signatures of data constructors
  , SourceType
  -- ^ The inferred kind of the declaration
  )

kindOfData
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> DataDeclarationArgs
  -> m DataDeclarationResult
kindOfData moduleName dataDecl =
  head . (^. _2) <$> kindsOfAll moduleName [] [dataDecl] []

inferDataDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> DataDeclarationArgs
  -> m [(DataConstructorDeclaration, SourceType)]
inferDataDeclaration moduleName (_, tyName, tyArgs, ctors) = do
  tyKind <- lookupTypeVariable moduleName (Qualified Nothing tyName)
  let (sigBinders, tyKind') = fromJust . completeBinderList $ tyKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    tyArgs' <- for tyArgs . traverse . maybe freshKind $ flip checkKind E.kindType
    unifyKinds tyKind' $ foldr ((E.-:>) . snd) E.kindType tyArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> tyArgs') $ do
      let tyCtorName = srcTypeConstructor $ mkQualified tyName moduleName
          tyCtor = foldl (\ty -> srcKindApp ty . srcTypeVar . fst . snd) tyCtorName sigBinders
          tyCtor' = foldl (\ty -> srcTypeApp ty . srcTypeVar . fst) tyCtor tyArgs'
          ctorBinders = fmap (fmap (fmap Just)) $ sigBinders <> fmap (nullSourceAnn,) tyArgs'
      for ctors $ \ctor ->
        fmap ((ctor,) . mkForAll ctorBinders) $ inferDataConstructor tyCtor' ctor

inferDataConstructor
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceType
  -> DataConstructorDeclaration
  -> m SourceType
inferDataConstructor tyCtor =
  flip checkKind E.kindType . foldr ((E.-:>) . snd) tyCtor . dataCtorFields

type TypeDeclarationArgs =
  ( SourceAnn
  , ProperName 'TypeName
  , [(Text, Maybe SourceType)]
  , SourceType
  )

type TypeDeclarationResult =
  ( SourceType
  -- ^ The elaborated rhs of the declaration
  , SourceType
  -- ^ The inferred kind of the declaration
  )

kindOfTypeSynonym
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> TypeDeclarationArgs
  -> m TypeDeclarationResult
kindOfTypeSynonym moduleName typeDecl =
  head . (^. _1) <$> kindsOfAll moduleName [typeDecl] [] []

inferTypeSynonym
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> TypeDeclarationArgs
  -> m SourceType
inferTypeSynonym moduleName (_, tyName, tyArgs, tyBody) = do
  tyKind <- lookupTypeVariable moduleName (Qualified Nothing tyName)
  let (sigBinders, tyKind') = fromJust . completeBinderList $ tyKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    kindRes <- freshKind
    tyArgs' <- for tyArgs . traverse . maybe freshKind $ flip checkKind E.kindType
    unifyKinds tyKind' $ foldr ((E.-:>) . snd) kindRes tyArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> tyArgs') $ do
      body <- inferKind tyBody
      apply =<< instantiateKind body =<< apply kindRes

type ClassDeclarationArgs =
  ( SourceAnn
  , ProperName 'ClassName
  , [(Text, Maybe SourceType)]
  , [SourceConstraint]
  , [Declaration]
  )

type ClassDeclarationResult =
  ( [(Text, SourceType)]
  -- ^ The kind annotated class arguments
  , SourceType
  -- ^ The inferred kind of the declaration
  )

kindOfClass
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> ClassDeclarationArgs
  -> m ClassDeclarationResult
kindOfClass moduleName clsDecl =
  head . (^. _3) <$> kindsOfAll moduleName [] [] [clsDecl]

inferClassDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> ClassDeclarationArgs
  -> m [(Text, SourceType)]
inferClassDeclaration moduleName (_, clsName, clsArgs, superClasses, decls) = do
  clsKind <- lookupTypeVariable moduleName (Qualified Nothing $ coerceProperName clsName)
  let (sigBinders, clsKind') = fromJust . completeBinderList $ clsKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    clsArgs' <- for clsArgs . traverse . maybe freshKind $ flip checkKind E.kindType
    unifyKinds clsKind' $ foldr ((E.-:>) . snd) E.kindConstraint clsArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> clsArgs') $ do
      for_ superClasses $ void . checkConstraint
      for_ decls $ void . checkClassMemberDeclaration
      pure clsArgs'

checkClassMemberDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => Declaration
  -> m SourceType
checkClassMemberDeclaration = \case
  TypeDeclaration (TypeDeclarationData _ _ ty) -> checkKind ty E.kindType
  _ -> internalError "Invalid class member declaration"

checkConstraint
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceConstraint
  -> m SourceType
checkConstraint (Constraint ann clsName args _) = do
  let ty = foldl (TypeApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) args
  checkKind ty E.kindConstraint

type InstanceDeclarationArgs =
  ( SourceAnn
  , [SourceConstraint]
  , Qualified (ProperName 'ClassName)
  , [SourceType]
  )

type InstanceDeclarationResult =
  [SourceType]

checkInstanceDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> InstanceDeclarationArgs
  -> m InstanceDeclarationResult
checkInstanceDeclaration _ (ann, constraints, clsName, args) = do
  let ty = foldl (TypeApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) args
  (_, args') <- unapplyTypes <$> checkKind ty E.kindConstraint
  for_ constraints $ void . checkConstraint
  for args' apply

kindsOfAll
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> [TypeDeclarationArgs]
  -> [DataDeclarationArgs]
  -> [ClassDeclarationArgs]
  -> m ([TypeDeclarationResult], [DataDeclarationResult], [ClassDeclarationResult])
kindsOfAll moduleName syns dats clss = withFreshSubstitution $ do
  synDict <- for syns $ \(_, synName, _, _) -> fmap (synName,) freshKind
  datDict <- for dats $ \(_, datName, _, _) -> fmap (datName,) freshKind
  clsDict <- for clss $ \(_, clsName, _, _, _) -> fmap (coerceProperName clsName,) freshKind
  let bindingGroup = synDict <> datDict <> clsDict
  bindLocalTypeVariables moduleName bindingGroup $ do
    synResults <- for syns (inferTypeSynonym moduleName)
    datResults <- for dats (inferDataDeclaration moduleName)
    clsResults <- for clss (inferClassDeclaration moduleName)
    synResultsWithUnks <- for (zip synDict synResults) $ \((synName, synKind), synBody) -> do
      synKind' <- apply synKind
      pure (((synName, synKind'), synBody), unknowns synKind')
    datResultsWithUnks <- for (zip datDict datResults) $ \((datName, datKind), ctors) -> do
      datKind' <- apply datKind
      ctors' <- traverse (traverse apply) ctors
      pure (((datName, datKind'), ctors'), unknowns datKind')
    clsResultsWithUnks <- for (zip clsDict clsResults) $ \((clsName, clsKind), args) -> do
      clsKind' <- apply clsKind
      args' <- traverse (traverse apply) args
      pure (((clsName, clsKind'), args'), unknowns clsKind')
    let synUnks = fmap (\(((synName, _), _), unks) -> (synName, unks)) synResultsWithUnks
        datUnks = fmap (\(((datName, _), _), unks) -> (datName, unks)) datResultsWithUnks
        clsUnks = fmap (\(((clsName, _), _), unks) -> (clsName, unks)) clsResultsWithUnks
        tysUnks = synUnks <> datUnks <> clsUnks
    allUnks <- unknownsWithKinds . IS.toList $ foldMap snd tysUnks
    let mkTySub (name, unks) = do
          let tyCtorName = mkQualified name moduleName
              tyUnks = filter (flip IS.member unks . fst) allUnks
              tyCtor = foldl (\ty -> srcKindApp ty . TUnknown nullSourceAnn . fst) (srcTypeConstructor tyCtorName) tyUnks
          (tyCtorName, (tyCtor, tyUnks))
        tySubs = fmap mkTySub tysUnks
        replaceTypeCtors = everywhereOnTypes $ \case
          TypeConstructor _ name
            | Just (tyCtor, _) <- lookup name tySubs -> tyCtor
          other -> other
        synResultsWithKinds = flip fmap synResultsWithUnks $ \(((synName, synKind), synBody), _) -> do
          let tyUnks = snd . fromJust $ lookup (mkQualified synName moduleName) tySubs
              unkBinders = unknownVarsForType tyUnks synKind
          (replaceUnknownsWithVars unkBinders $ replaceTypeCtors synBody, generalizeUnknownsWithVars unkBinders synKind)
        datResultsWithKinds = flip fmap datResultsWithUnks $ \(((datName, datKind), ctors), _) -> do
          let tyUnks = snd . fromJust $ lookup (mkQualified datName moduleName) tySubs
              ctors' = fmap (fmap (generalizeUnknowns tyUnks . replaceTypeCtors)) ctors
          (ctors', generalizeUnknowns tyUnks datKind)
        clsResultsWithKinds = flip fmap clsResultsWithUnks $ \(((clsName, clsKind), args), _) -> do
          let tyUnks = snd . fromJust $ lookup (mkQualified clsName moduleName) tySubs
          (args, generalizeUnknowns tyUnks clsKind)
    pure (synResultsWithKinds, datResultsWithKinds, clsResultsWithKinds)
