{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- |
-- This module implements the kind checker
--
module Language.PureScript.TypeChecker.Kinds
  ( kindOf
  , kindOfWithUnknowns
  , kindOfWithScopedVars
  , kindOfData
  , kindOfTypeSynonym
  , kindOfClass
  , kindsOfAll
  , unifyKinds
  , checkKind
  , inferKind
  , checkConstraint
  , checkInstanceDeclaration
  , checkKindDeclaration
  , checkTypeKind
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State

import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.IntSet as IS
import Data.List (nubBy, sortBy, (\\))
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
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Types
import Language.PureScript.Pretty.Types
import Lens.Micro.Platform ((^.), _1, _2, _3)

generalizeUnknowns :: [(Unknown, SourceType)] -> SourceType -> SourceType
generalizeUnknowns unks ty =
  generalizeUnknownsWithVars (unknownVarsForType unks ty) ty

generalizeUnknownsWithVars :: [(Unknown, (Text, SourceType))] -> SourceType -> SourceType
generalizeUnknownsWithVars binders ty =
  mkForAll ((getAnnForType ty,) . fmap Just . snd <$> binders) . replaceUnknownsWithVars binders $ ty

replaceUnknownsWithVars :: [(Unknown, (Text, a))] -> SourceType -> SourceType
replaceUnknownsWithVars binders ty
  | null binders = ty
  | otherwise = go ty
  where
  go :: SourceType -> SourceType
  go = everywhereOnTypes $ \case
    TUnknown ann unk | Just (name, _) <- lookup unk binders -> TypeVar ann name
    other -> other

unknownVarsForType :: [(Unknown, SourceType)] -> SourceType -> [(Unknown, (Text, SourceType))]
unknownVarsForType unks ty =
  zipWith (\(a, b) n -> (a, (n, b))) unks $ allVars \\ usedTypeVariables ty
  where
  allVars :: [Text]
  allVars
    | [_] <- unks = "k" : vars
    | otherwise = vars

  vars :: [Text]
  vars = fmap (("k" <>) . T.pack . show) ([1..] :: [Int])

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
    Nothing -> internalError $ "Unsolved unification variable ?" <> show u <> " is not bound"
    Just res -> return res

unknownsWithKinds
  :: forall m. (MonadState CheckState m, MonadError MultipleErrors m, HasCallStack)
  => [Unknown]
  -> m [(Unknown, SourceType)]
unknownsWithKinds = fmap (fmap snd . nubBy ((==) `on` fst) . sortBy (comparing fst) . join) . traverse go
  where
  go u = do
    (lvl, ty) <- traverse apply =<< lookupUnsolved u
    rest <- fmap join . traverse go . IS.toList . unknowns $ ty
    pure $ (lvl, (u, ty)) : rest

inferKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (SourceType, SourceType)
inferKind = \tyToInfer ->
  withErrorMessageHint (ErrorInferringKind tyToInfer)
    . rethrowWithPosition (fst $ getAnnForType tyToInfer)
    $ go tyToInfer
  where
  go = \case
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
    ConstrainedType ann' con@(Constraint ann v _ _ _) ty -> do
      env <- getEnv
      con' <- case M.lookup (coerceProperName <$> v) (E.types env) of
        Nothing ->
          throwError . errorMessage' (fst ann) . UnknownName . fmap TyClassName $ v
        Just _ ->
          checkConstraint con
      ty' <- checkKind ty E.kindType
      con'' <- applyConstraint con'
      pure (ConstrainedType ann' con'' ty', E.kindType $> ann')
    ty@(TypeLevelString ann _) ->
      pure (ty, E.kindSymbol $> ann)
    ty@(TypeVar ann v) -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- apply =<< lookupTypeVariable moduleName (Qualified Nothing $ ProperName v)
      pure (ty, kind $> ann)
    ty@(Skolem ann _ mbK _ _) -> do
      kind <- apply $ maybe (internalError "Skolem has no kind") id mbK
      pure (ty, kind $> ann)
    ty@(TUnknown ann u) -> do
      kind <- apply . snd =<< lookupUnsolved u
      pure (ty, kind $> ann)
    ty@(TypeWildcard ann _) -> do
      k <- freshKind
      pure (ty, k $> ann)
    ty@(REmpty ann) -> do
      pure (ty, E.kindOfREmpty $> ann)
    ty@(RCons ann _ _ _) | (rowList, rowTail) <- rowToList ty -> do
      kr <- freshKind
      rowList' <- for rowList $ \(RowListItem a lbl t) ->
        RowListItem a lbl <$> checkKind t kr
      rowTail' <- checkKind rowTail $ E.kindRow kr
      kr' <- apply kr
      pure (rowFromList (rowList', rowTail'), E.kindRow kr' $> ann)
    TypeApp ann t1 t2 -> do
      (t1', k1) <- go t1
      inferAppKind ann (t1', k1) t2
    KindApp ann t1 t2 -> do
      (t1', kind) <- bitraverse pure apply =<< go t1
      case kind of
        ForAll _ arg (Just argKind) resKind _ -> do
          t2' <- checkKind t2 argKind
          pure (KindApp ann t1' t2', replaceTypeVars arg t2' resKind)
        _ ->
          internalError $ "inferKind: unkinded forall binder"
    KindedType _ t1 t2 -> do
      t2' <- replaceAllTypeSynonyms . fst =<< go t2
      t1' <- checkKind t1 t2'
      t2'' <- apply t2'
      pure (t1', t2'')
    ForAll ann arg mbKind ty sc -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- case mbKind of
        Just k -> replaceAllTypeSynonyms =<< checkKind k E.kindType
        Nothing -> fmap ($> ann) freshKind
      (ty', unks) <- bindLocalTypeVariables moduleName [(ProperName arg, kind)] $ do
        ty' <- apply =<< checkKind ty E.kindType
        unks <- for (IS.toList $ unknowns ty') $ \u ->
          fmap (u,) . apply . snd =<< lookupUnsolved u
        pure (ty', unks)
      unless (not . elem arg $ foldMap (freeTypeVariables . snd) unks) $
        throwError . errorMessage' (fst ann) $ QuantificationCheckFailure arg
      for_ unks . uncurry $ addUnsolved Nothing
      pure (ForAll ann arg (Just kind) ty' sc, E.kindType $> ann)
    ParensInType _ ty ->
      go ty
    ty ->
      internalError $ "inferKind: Unimplemented case \n" <> prettyPrintType 100 ty

inferAppKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceAnn
  -> (SourceType, SourceType)
  -> SourceType
  -> m (SourceType, SourceType)
inferAppKind ann (fn, fnKind) arg = case fnKind of
  TypeApp _ (TypeApp _ arrKind argKind) resKind | eqType arrKind E.tyFunction -> do
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
  _ ->
    cannotApplyTypeToType fn arg

cannotApplyTypeToType
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m a
cannotApplyTypeToType fn arg = do
  argKind <- snd <$> inferKind arg
  _ <- checkKind fn . srcTypeApp (srcTypeApp E.tyFunction argKind) =<< freshKind
  internalError "Cannot apply type to type"

cannotApplyKindToType
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m a
cannotApplyKindToType poly arg = do
  let ann = getAnnForType arg
  argKind <- snd <$> inferKind arg
  _ <- checkKind poly . mkForAll [(ann, ("k", Just argKind))] =<< freshKind
  internalError "Cannot apply kind to type"

checkKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m SourceType
checkKind ty kind2 =
  withErrorMessageHint (ErrorCheckingKind ty kind2)
    . rethrowWithPosition (fst $ getAnnForType ty) $ do
        (ty', kind1) <- inferKind ty
        kind1' <- apply kind1
        kind2' <- apply kind2
        instantiateKind (ty', kind1') kind2'

instantiateKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
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
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m ()
unifyKinds = unifyKindsWithFailure $ \w1 w2 ->
  throwError
    . errorMessage''' (fst . getAnnForType <$> [w1, w2])
    $ KindsDoNotUnify w1 w2

-- | Check the kind of a type, failing if it is not of kind *.
checkTypeKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> SourceType
  -> m ()
checkTypeKind ty kind =
  unifyKindsWithFailure (\_ _ -> throwError . errorMessage $ ExpectedType ty kind) kind E.kindType

unifyKindsWithFailure
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => (SourceType -> SourceType -> m ())
  -> SourceType
  -> SourceType
  -> m ()
unifyKindsWithFailure onFailure = go
  where
  go = curry $ \case
    (TypeApp _ p1 p2, TypeApp _ p3 p4) -> do
      go p1 p3
      join $ go <$> apply p2 <*> apply p4
    (KindApp _ p1 p2, KindApp _ p3 p4) -> do
      go p1 p3
      join $ go <$> apply p2 <*> apply p4
    (r1@(RCons _ _ _ _), r2) ->
      unifyRows r1 r2
    (r1, r2@(RCons _ _ _ _)) ->
      unifyRows r1 r2
    (r1@(REmpty _), r2) ->
      unifyRows r1 r2
    (r1, r2@(REmpty _)) ->
      unifyRows r1 r2
    (w1, w2) | eqType w1 w2 ->
      pure ()
    (TUnknown _ a', p1) ->
      solveUnknown a' p1
    (p1, TUnknown _ a') ->
      solveUnknown a' p1
    (w1, w2) ->
      onFailure w1 w2

  unifyRows r1 r2 = do
    let (matches, rest) = alignRowsWith go r1 r2
    sequence_ matches
    unifyTails rest

  unifyTails = \case
    (([], TUnknown _ a'), (rs, p1)) ->
      solveUnknown a' $ rowFromList (rs, p1)
    ((rs, p1), ([], TUnknown _ a')) ->
      solveUnknown a' $ rowFromList (rs, p1)
    (([], w1), ([], w2)) | eqType w1 w2 ->
      pure ()
    ((rs1, TUnknown _ u1), (rs2, TUnknown _ u2)) -> do
      rest <- freshKind
      solveUnknown u1 $ rowFromList (rs2, rest)
      solveUnknown u2 $ rowFromList (rs1, rest)
    (w1, w2) ->
      onFailure (rowFromList w1) (rowFromList w2)

solveUnknown
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => Unknown
  -> SourceType
  -> m ()
solveUnknown a' p1 = do
  p2 <- promoteKind a' p1
  w1 <- snd <$> lookupUnsolved a'
  join $ unifyKinds <$> apply w1 <*> elaborateKind p2
  solve a' p2

promoteKind
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => Unknown
  -> SourceType
  -> m SourceType
promoteKind u2 ty = do
  lvl2 <- fst <$> lookupUnsolved u2
  flip everywhereOnTypesM ty $ \case
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
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m SourceType
elaborateKind = \case
  TypeLevelString ann _ ->
    pure $ E.kindSymbol $> ann
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
  (Skolem ann _ mbK _ _) -> do
    kind <- apply $ maybe (internalError "Skolem has no kind") id mbK
    pure $ kind $> ann
  TUnknown ann a' -> do
    kind <- snd <$> lookupUnsolved a'
    ($> ann) <$> apply kind
  REmpty ann -> do
    pure $ E.kindOfREmpty $> ann
  ty@(RCons ann _ _ _) -> do
    let (rl, _) = rowToList ty
    ks <- traverse (elaborateKind . rowListType) rl
    unless (all (eqType (head ks)) $ tail ks) $
      internalError $ "elaborateKind: Elaborated row kinds are not all " <> prettyPrintType 10 (head ks)
    pure $ E.kindRow (head ks) $> ann
  TypeApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      TypeApp _ (TypeApp _ k w1) w2 | eqType k E.tyFunction -> do
        k2 <- elaborateKind t2
        unless (eqType k2 w1) $ cannotApplyTypeToType t1 t2
        pure $ w2 $> ann
      _ ->
        cannotApplyTypeToType t1 t2
  KindApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      ForAll _ a (Just w) n _ -> do
        k2 <- elaborateKind t2
        unless (eqType k2 w) $ cannotApplyKindToType t1 t2
        flip (replaceTypeVars a) n . ($> ann) <$> apply t2
      _ ->
        cannotApplyKindToType t1 t2
  ForAll ann a (Just w) u _ -> do
    wIsStar <- elaborateKind w
    unless (eqType wIsStar E.kindType) $
      internalError $ "elaborateKind: Elaborated kind is not Type " <> prettyPrintType 10 wIsStar
    moduleName <- unsafeCheckCurrentModule
    uIsStar <- bindLocalTypeVariables moduleName [(ProperName a, w)] $ elaborateKind u
    unless (eqType uIsStar E.kindType) $
      internalError $ "elaborateKind: Elaborated kind is not Type " <> prettyPrintType 10 uIsStar
    pure $ E.kindType $> ann
  ty ->
    internalError $ "elaborateKind: Unimplemented case " <> prettyPrintType 10 ty

kindOfWithUnknowns
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (([(Unknown, SourceType)], SourceType), SourceType)
kindOfWithUnknowns ty = do
  (ty', kind) <- kindOf ty
  unks <- unknownsWithKinds . IS.toList $ unknowns ty'
  pure ((unks, ty'), kind)

-- | Infer the kind of a single type
kindOf
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (SourceType, SourceType)
kindOf = fmap (first snd) . kindOfWithScopedVars

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars
  :: (MonadError MultipleErrors m, MonadState CheckState m, HasCallStack)
  => SourceType
  -> m (([(Text, SourceType)], SourceType), SourceType)
kindOfWithScopedVars ty = do
  (ty', kind) <- bitraverse apply (replaceAllTypeSynonyms <=< apply) =<< inferKind ty
  let binders = fst . fromJust $ completeBinderList ty'
  pure ((snd <$> binders, ty'), kind)

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
    tyArgs' <- for tyArgs . traverse . maybe freshKind $ replaceAllTypeSynonyms <=< apply <=< flip checkKind E.kindType
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
    tyArgs' <- for tyArgs . traverse . maybe freshKind $ replaceAllTypeSynonyms <=< apply <=< flip checkKind E.kindType
    unifyKinds tyKind' $ foldr ((E.-:>) . snd) kindRes tyArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> tyArgs') $ do
      tyBodyAndKind <- inferKind tyBody
      tyBody' <- apply =<< instantiateKind tyBodyAndKind =<< apply kindRes
      unks <- unknownsWithKinds . IS.toList $ unknowns tyBody'
      pure $ generalizeUnknowns unks tyBody'

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
  , [SourceConstraint]
  -- ^ The kind annotated superclass constraints
  , [Declaration]
  -- ^ The kind annotated declarations
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
  -> m ([(Text, SourceType)], [SourceConstraint], [Declaration])
inferClassDeclaration moduleName (_, clsName, clsArgs, superClasses, decls) = do
  clsKind <- lookupTypeVariable moduleName (Qualified Nothing $ coerceProperName clsName)
  let (sigBinders, clsKind') = fromJust . completeBinderList $ clsKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    clsArgs' <- for clsArgs . traverse . maybe freshKind $ replaceAllTypeSynonyms <=< apply <=< flip checkKind E.kindType
    unifyKinds clsKind' $ foldr ((E.-:>) . snd) E.kindConstraint clsArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> clsArgs') $ do
      (clsArgs',,)
        <$> for superClasses checkConstraint
        <*> for decls checkClassMemberDeclaration

checkClassMemberDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => Declaration
  -> m Declaration
checkClassMemberDeclaration = \case
  TypeDeclaration (TypeDeclarationData ann ident ty) ->
    TypeDeclaration . TypeDeclarationData ann ident <$> checkKind ty E.kindType
  _ -> internalError "Invalid class member declaration"

applyClassMemberDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => Declaration
  -> m Declaration
applyClassMemberDeclaration = \case
  TypeDeclaration (TypeDeclarationData ann ident ty) ->
    TypeDeclaration . TypeDeclarationData ann ident <$> apply ty
  _ -> internalError "Invalid class member declaration"

checkConstraint
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceConstraint
  -> m SourceConstraint
checkConstraint (Constraint ann clsName kinds args dat) = do
  let ty = foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args
  (_, kinds', args') <- unapplyTypes <$> checkKind ty E.kindConstraint
  pure $ Constraint ann clsName kinds' args' dat

applyConstraint
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => SourceConstraint
  -> m SourceConstraint
applyConstraint (Constraint ann clsName kinds args dat) = do
  let ty = foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args
  (_, kinds', args') <- unapplyTypes <$> apply ty
  pure $ Constraint ann clsName kinds' args' dat

type InstanceDeclarationArgs =
  ( SourceAnn
  , [SourceConstraint]
  , Qualified (ProperName 'ClassName)
  , [SourceType]
  )

type InstanceDeclarationResult =
  ([SourceConstraint], [SourceType], [SourceType])

checkInstanceDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> InstanceDeclarationArgs
  -> m InstanceDeclarationResult
checkInstanceDeclaration moduleName (ann, constraints, clsName, args) = do
  let ty = foldl (TypeApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) args
      tyWithConstraints = foldr srcConstrainedType ty constraints
      freeVars = freeTypeVariables tyWithConstraints
  freeVarsDict <- for freeVars $ \v -> (ProperName v,) <$> freshKind
  bindLocalTypeVariables moduleName freeVarsDict $ do
    ty' <- checkKind ty E.kindConstraint
    constraints' <- for constraints checkConstraint
    allTy <- apply $ foldr (\a b -> srcConstrainedType a b) ty' constraints'
    allUnknowns <- unknownsWithKinds . IS.toList $ unknowns allTy
    let allWithVars = replaceUnknownsWithVars (unknownVarsForType allUnknowns allTy) allTy
        (allConstraints, (_, allKinds, allArgs)) = unapplyTypes <$> unapplyConstraints allWithVars
    pure (allConstraints, allKinds, allArgs)

checkKindDeclaration
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> SourceType
  -> m SourceType
checkKindDeclaration _ ty = do
  (ty', kind) <- kindOf ty
  checkTypeKind kind E.kindType
  ty'' <- replaceAllTypeSynonyms ty'
  unks <- unknownsWithKinds . IS.toList $ unknowns ty''
  pure $ generalizeUnknowns unks ty''

existingSignatureOrFreshKind
  :: forall m. MonadState CheckState m
  => ModuleName
  -> ProperName 'TypeName
  -> m SourceType
existingSignatureOrFreshKind moduleName name = do
  env <- getEnv
  case M.lookup (Qualified (Just moduleName) name) (E.types env) of
    Nothing -> freshKind
    Just (kind, _) -> pure kind

kindsOfAll
  :: forall m. (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> [TypeDeclarationArgs]
  -> [DataDeclarationArgs]
  -> [ClassDeclarationArgs]
  -> m ([TypeDeclarationResult], [DataDeclarationResult], [ClassDeclarationResult])
kindsOfAll moduleName syns dats clss = withFreshSubstitution $ do
  synDict <- for syns $ \(_, synName, _, _) -> fmap (synName,) $ existingSignatureOrFreshKind moduleName synName
  datDict <- for dats $ \(_, datName, _, _) -> fmap (datName,) $ existingSignatureOrFreshKind moduleName datName
  clsDict <- for clss $ \(_, clsName, _, _, _) -> fmap (coerceProperName clsName,) $ existingSignatureOrFreshKind moduleName $ coerceProperName clsName
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
    clsResultsWithUnks <- for (zip clsDict clsResults) $ \((clsName, clsKind), (args, supers, decls)) -> do
      clsKind' <- apply clsKind
      args' <- traverse (traverse apply) args
      supers' <- traverse applyConstraint supers
      decls' <- traverse applyClassMemberDeclaration decls
      pure (((clsName, clsKind'), (args', supers', decls')), unknowns clsKind')
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
        clsResultsWithKinds = flip fmap clsResultsWithUnks $ \(((clsName, clsKind), (args, supers, decls)), _) -> do
          let tyUnks = snd . fromJust $ lookup (mkQualified clsName moduleName) tySubs
          (args, supers, decls, generalizeUnknowns tyUnks clsKind)
    pure (synResultsWithKinds, datResultsWithKinds, clsResultsWithKinds)
