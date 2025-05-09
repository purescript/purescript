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
  , unifyKinds'
  , subsumesKind
  , instantiateKind
  , checkKind
  , inferKind
  , elaborateKind
  , checkConstraint
  , checkInstanceDeclaration
  , checkKindDeclaration
  , checkTypeKind
  , unknownsWithKinds
  , freshKind
  , freshKindWithKind
  ) where

import Prelude

import Control.Arrow ((***))
import Control.Lens ((^.), _1, _2, _3)
import Control.Monad (join, unless, void, when, (<=<))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (gets, modify)
import Control.Monad.Supply.Class (MonadSupply(..))

import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor (($>))
import Data.IntSet qualified as IS
import Data.List (nubBy, sortOn, (\\))
import Data.Map qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)

import Language.PureScript.Crash (HasCallStack, internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Errors
import Language.PureScript.Names (pattern ByNullSourcePos, ModuleName, Name(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, mkQualified)
import Language.PureScript.TypeChecker.Monad (CheckState(..), Substitution(..), UnkLevel(..), Unknown, bindLocalTypeVariables, debugType, getEnv, lookupTypeVariable, unsafeCheckCurrentModule, withErrorMessageHint, withFreshSubstitution, TypeCheckM)
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, newSkolemScope, skolemize)
import Language.PureScript.TypeChecker.Synonyms (replaceAllTypeSynonyms)
import Language.PureScript.Types
import Language.PureScript.Pretty.Types (prettyPrintType)

generalizeUnknowns :: [(Unknown, SourceType)] -> SourceType -> SourceType
generalizeUnknowns unks ty =
  generalizeUnknownsWithVars (unknownVarNames (usedTypeVariables ty) unks) ty

generalizeUnknownsWithVars :: [(Unknown, (Text, SourceType))] -> SourceType -> SourceType
generalizeUnknownsWithVars binders ty =
  mkForAll ((getAnnForType ty,) . fmap (Just . replaceUnknownsWithVars binders) . snd <$> binders) . replaceUnknownsWithVars binders $ ty

replaceUnknownsWithVars :: [(Unknown, (Text, a))] -> SourceType -> SourceType
replaceUnknownsWithVars binders ty
  | null binders = ty
  | otherwise = go ty
  where
  go :: SourceType -> SourceType
  go = everywhereOnTypes $ \case
    TUnknown ann unk | Just (name, _) <- lookup unk binders -> TypeVar ann name
    other -> other

unknownVarNames :: [Text] -> [(Unknown, SourceType)] -> [(Unknown, (Text, SourceType))]
unknownVarNames used unks =
  zipWith (\(a, b) n -> (a, (n, b))) unks $ allVars \\ used
  where
  allVars :: [Text]
  allVars
    | [_] <- unks = "k" : vars
    | otherwise = vars

  vars :: [Text]
  vars = fmap (("k" <>) . T.pack . show) ([1..] :: [Int])

apply :: SourceType -> TypeCheckM SourceType
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

freshUnknown :: TypeCheckM Unknown
freshUnknown = do
  k <- gets checkNextType
  modify $ \st -> st { checkNextType = k + 1 }
  pure k

freshKind :: SourceSpan -> TypeCheckM SourceType
freshKind ss = freshKindWithKind ss E.kindType

freshKindWithKind :: SourceSpan -> SourceType -> TypeCheckM SourceType
freshKindWithKind ss kind = do
  u <- freshUnknown
  addUnsolved Nothing u kind
  pure $ TUnknown (ss, []) u

addUnsolved :: Maybe UnkLevel -> Unknown -> SourceType -> TypeCheckM ()
addUnsolved lvl unk kind = modify $ \st -> do
  let
    newLvl = UnkLevel $ case lvl of
      Nothing -> pure unk
      Just (UnkLevel lvl') -> lvl' <> pure unk
    subs = checkSubstitution st
    uns = M.insert unk (newLvl, kind) $ substUnsolved subs
  st { checkSubstitution = subs { substUnsolved = uns } }

solve :: Unknown -> SourceType -> TypeCheckM ()
solve unk solution = modify $ \st -> do
  let
    subs = checkSubstitution st
    tys = M.insert unk solution $ substType subs
  st { checkSubstitution = subs { substType = tys } }

lookupUnsolved
  :: (HasCallStack)
  => Unknown
  -> TypeCheckM (UnkLevel, SourceType)
lookupUnsolved u = do
  uns <- gets (substUnsolved . checkSubstitution)
  case M.lookup u uns of
    Nothing -> internalCompilerError $ "Unsolved unification variable ?" <> T.pack (show u) <> " is not bound"
    Just res -> return res

unknownsWithKinds
  :: (HasCallStack)
  => [Unknown]
  -> TypeCheckM [(Unknown, SourceType)]
unknownsWithKinds = fmap (fmap snd . nubBy ((==) `on` fst) . sortOn fst . join) . traverse go
  where
  go u = do
    (lvl, ty) <- traverse apply =<< lookupUnsolved u
    rest <- fmap join . traverse go . IS.toList . unknowns $ ty
    pure $ (lvl, (u, ty)) : rest

inferKind
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM (SourceType, SourceType)
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
      ty' <- checkIsSaturatedType ty
      con'' <- applyConstraint con'
      pure (ConstrainedType ann' con'' ty', E.kindType $> ann')
    ty@(TypeLevelString ann _) ->
      pure (ty, E.kindSymbol $> ann)
    ty@(TypeLevelInt ann _) ->
      pure (ty, E.tyInt $> ann)
    ty@(TypeVar ann v) -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- apply =<< lookupTypeVariable moduleName (Qualified ByNullSourcePos $ ProperName v)
      pure (ty, kind $> ann)
    ty@(Skolem ann _ mbK _ _) -> do
      kind <- apply $ fromMaybe (internalError "Skolem has no kind") mbK
      pure (ty, kind $> ann)
    ty@(TUnknown ann u) -> do
      kind <- apply . snd =<< lookupUnsolved u
      pure (ty, kind $> ann)
    ty@(TypeWildcard ann _) -> do
      k <- freshKind (fst ann)
      pure (ty, k $> ann)
    ty@(REmpty ann) -> do
      pure (ty, E.kindOfREmpty $> ann)
    ty@(RCons ann _ _ _) | (rowList, rowTail) <- rowToList ty -> do
      kr <- freshKind (fst ann)
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
        ForAll _ _ arg (Just argKind) resKind _ -> do
          t2' <- checkKind t2 argKind
          pure (KindApp ann t1' t2', replaceTypeVars arg t2' resKind)
        _ ->
          internalError "inferKind: unkinded forall binder"
    KindedType _ t1 t2 -> do
      t2' <- replaceAllTypeSynonyms . fst =<< go t2
      t1' <- checkKind t1 t2'
      t2'' <- apply t2'
      pure (t1', t2'')
    ForAll ann vis arg mbKind ty sc -> do
      moduleName <- unsafeCheckCurrentModule
      kind <- case mbKind of
        Just k -> replaceAllTypeSynonyms =<< checkIsSaturatedType k
        Nothing -> freshKind (fst ann)
      (ty', unks) <- bindLocalTypeVariables moduleName [(ProperName arg, kind)] $ do
        ty' <- apply =<< checkIsSaturatedType ty
        unks <- unknownsWithKinds . IS.toList $ unknowns ty'
        pure (ty', unks)
      for_ unks . uncurry $ addUnsolved Nothing
      pure (ForAll ann vis arg (Just kind) ty' sc, E.kindType $> ann)
    ParensInType _ ty ->
      go ty
    ty ->
      internalError $ "inferKind: Unimplemented case \n" <> prettyPrintType 100 ty

inferAppKind
  :: (HasCallStack)
  => SourceAnn
  -> (SourceType, SourceType)
  -> SourceType
  -> TypeCheckM (SourceType, SourceType)
inferAppKind ann (fn, fnKind) arg = case fnKind of
  TypeApp _ (TypeApp _ arrKind argKind) resKind | eqType arrKind E.tyFunction -> do
    expandSynonyms <- requiresSynonymsToExpand fn
    arg' <- checkKind' expandSynonyms arg argKind
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
  ForAll _ _ a (Just k) ty _ -> do
    u <- freshUnknown
    addUnsolved Nothing u k
    inferAppKind ann (KindApp ann fn (TUnknown ann u), replaceTypeVars a (TUnknown ann u) ty) arg
  _ ->
    cannotApplyTypeToType fn arg
  where
  requiresSynonymsToExpand = \case
    TypeConstructor _ v -> M.notMember v . E.typeSynonyms <$> getEnv
    TypeApp _ l _ -> requiresSynonymsToExpand l
    KindApp _ l _ -> requiresSynonymsToExpand l
    _ -> pure True

cannotApplyTypeToType
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM a
cannotApplyTypeToType fn arg = do
  argKind <- snd <$> inferKind arg
  _ <- checkKind fn . srcTypeApp (srcTypeApp E.tyFunction argKind) =<< freshKind nullSourceSpan
  internalCompilerError . T.pack $ "Cannot apply type to type: " <> debugType (srcTypeApp fn arg)

cannotApplyKindToType
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM a
cannotApplyKindToType poly arg = do
  let ann = getAnnForType arg
  argKind <- snd <$> inferKind arg
  _ <- checkKind poly . mkForAll [(ann, ("k", Just argKind))] =<< freshKind nullSourceSpan
  internalCompilerError . T.pack $ "Cannot apply kind to type: " <> debugType (srcKindApp poly arg)

checkKind
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM SourceType
checkKind = checkKind' False

-- | `checkIsSaturatedType t` is identical to `checkKind t E.kindType` except
-- that the former checks that the type synonyms in `t` expand completely. This
-- is the appropriate function to use when expanding the types of type
-- parameter kinds, arguments to data constructors, etc., in order for the
-- PartiallyAppliedSynonym error to take precedence over the KindsDoNotUnify
-- error.
--
checkIsSaturatedType
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM SourceType
checkIsSaturatedType ty = checkKind' True ty E.kindType

checkKind'
  :: (HasCallStack)
  => Bool
  -> SourceType
  -> SourceType
  -> TypeCheckM SourceType
checkKind' requireSynonymsToExpand ty kind2 = do
  withErrorMessageHint (ErrorCheckingKind ty kind2)
    . rethrowWithPosition (fst $ getAnnForType ty) $ do
        (ty', kind1) <- inferKind ty
        kind1' <- apply kind1
        kind2' <- apply kind2
        when requireSynonymsToExpand $ void $ replaceAllTypeSynonyms ty'
        instantiateKind (ty', kind1') kind2'

instantiateKind
  :: (HasCallStack)
  => (SourceType, SourceType)
  -> SourceType
  -> TypeCheckM SourceType
instantiateKind (ty, kind1) kind2 = case kind1 of
  ForAll _ _ a (Just k) t _ | shouldInstantiate kind2 -> do
    let ann = getAnnForType ty
    u <- freshKindWithKind (fst ann) k
    instantiateKind (KindApp ann ty u, replaceTypeVars a u t) kind2
  _ -> do
    subsumesKind kind1 kind2
    pure ty
  where
  shouldInstantiate = not . \case
    ForAll _ _ _ _ _ _ -> True
    _ -> False

subsumesKind
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM ()
subsumesKind = go
  where
  go = curry $ \case
    (TypeApp _ (TypeApp _ arr1 a1) a2, TypeApp _ (TypeApp _ arr2 b1) b2)
      | eqType arr1 E.tyFunction
      , eqType arr2 E.tyFunction -> do
          go b1 a1
          join $ go <$> apply a2 <*> apply b2
    (a, ForAll ann _ var mbKind b mbScope) -> do
      scope <- maybe newSkolemScope pure mbScope
      skolc <- newSkolemConstant
      go a $ skolemize ann var mbKind skolc scope b
    (ForAll ann _ var (Just kind) a _, b) -> do
      a' <- freshKindWithKind (fst ann) kind
      go (replaceTypeVars var a' a) b
    (TUnknown ann u, b@(TypeApp _ (TypeApp _ arr _) _))
      | eqType arr E.tyFunction
      , IS.notMember u (unknowns b) ->
          join $ go <$> solveUnknownAsFunction ann u <*> pure b
    (a@(TypeApp _ (TypeApp _ arr _) _), TUnknown ann u)
      | eqType arr E.tyFunction
      , IS.notMember u (unknowns a) ->
          join $ go <$> pure a <*> solveUnknownAsFunction ann u
    (a, b) ->
      unifyKinds a b

unifyKinds
  :: SourceType
  -> SourceType
  -> TypeCheckM ()
unifyKinds = unifyKindsWithFailure $ \w1 w2 ->
  throwError
    . errorMessage''' (fst . getAnnForType <$> [w1, w2])
    $ KindsDoNotUnify w1 w2

-- | Does not attach positions to the error node, instead relies on the
-- | local position context. This is useful when invoking kind unification
-- | outside of kind checker internals.
unifyKinds'
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM ()
unifyKinds' = unifyKindsWithFailure $ \w1 w2 ->
  throwError
    . errorMessage
    $ KindsDoNotUnify w1 w2

-- | Check the kind of a type, failing if it is not of kind *.
checkTypeKind
  :: (HasCallStack)
  => SourceType
  -> SourceType
  -> TypeCheckM ()
checkTypeKind ty kind =
  unifyKindsWithFailure (\_ _ -> throwError . errorMessage $ ExpectedType ty kind) kind E.kindType

unifyKindsWithFailure
  :: (HasCallStack)
  => (SourceType -> SourceType -> TypeCheckM ())
  -> SourceType
  -> SourceType
  -> TypeCheckM ()
unifyKindsWithFailure onFailure = go
  where
  goWithLabel l t1 t2 = withErrorMessageHint (ErrorInRowLabel l) $ go t1 t2
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
    let (matches, rest) = alignRowsWith goWithLabel r1 r2
    sequence_ matches
    unifyTails rest

  unifyTails = \case
    (([], TUnknown _ a'), (rs, p1)) ->
      solveUnknown a' $ rowFromList (rs, p1)
    ((rs, p1), ([], TUnknown _ a')) ->
      solveUnknown a' $ rowFromList (rs, p1)
    (([], w1), ([], w2)) | eqType w1 w2 ->
      pure ()
    ((rs1, TUnknown _ u1), (rs2, TUnknown _ u2)) | u1 /= u2 -> do
      rest <- freshKind nullSourceSpan
      solveUnknown u1 $ rowFromList (rs2, rest)
      solveUnknown u2 $ rowFromList (rs1, rest)
    (w1, w2) ->
      onFailure (rowFromList w1) (rowFromList w2)

solveUnknown
  :: (HasCallStack)
  => Unknown
  -> SourceType
  -> TypeCheckM ()
solveUnknown a' p1 = do
  p2 <- promoteKind a' p1
  w1 <- snd <$> lookupUnsolved a'
  join $ unifyKinds <$> apply w1 <*> elaborateKind p2
  solve a' p2

solveUnknownAsFunction
  :: (HasCallStack)
  => SourceAnn
  -> Unknown
  -> TypeCheckM SourceType
solveUnknownAsFunction ann u = do
  lvl <- fst <$> lookupUnsolved u
  u1 <- freshUnknown
  u2 <- freshUnknown
  addUnsolved (Just lvl) u1 E.kindType
  addUnsolved (Just lvl) u2 E.kindType
  let uarr = (TUnknown ann u1 E.-:> TUnknown ann u2) $> ann
  solve u uarr
  pure uarr

promoteKind
  :: (HasCallStack)
  => Unknown
  -> SourceType
  -> TypeCheckM SourceType
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
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM SourceType
elaborateKind = \case
  TypeLevelString ann _ ->
    pure $ E.kindSymbol $> ann
  TypeLevelInt ann _ ->
    pure $ E.tyInt $> ann
  TypeConstructor ann v -> do
    env <- getEnv
    case M.lookup v (E.types env) of
      Nothing ->
        throwError . errorMessage' (fst ann) . UnknownName . fmap TyName $ v
      Just (kind, _) ->
        ($> ann) <$> apply kind
  TypeVar ann a -> do
    moduleName <- unsafeCheckCurrentModule
    kind <- apply =<< lookupTypeVariable moduleName (Qualified ByNullSourcePos $ ProperName a)
    pure (kind $> ann)
  (Skolem ann _ mbK _ _) -> do
    kind <- apply $ fromMaybe (internalError "Skolem has no kind") mbK
    pure $ kind $> ann
  TUnknown ann a' -> do
    kind <- snd <$> lookupUnsolved a'
    ($> ann) <$> apply kind
  REmpty ann -> do
    pure $ E.kindOfREmpty $> ann
  RCons ann _ t1 _ -> do
    k1 <- elaborateKind t1
    pure $ E.kindRow k1 $> ann
  ty@(TypeApp ann t1 t2) -> do
    k1 <- elaborateKind t1
    case k1 of
      TypeApp _ (TypeApp _ k _) w2 | eqType k E.tyFunction -> do
        pure $ w2 $> ann
      -- Normally we wouldn't unify in `elaborateKind`, since an unknown should
      -- always have a known kind. However, since type holes are fully inference
      -- driven, they are unknowns with unknown kinds, which may require some
      -- late unification here.
      TUnknown a u -> do
        _ <- solveUnknownAsFunction a u
        elaborateKind ty
      _ ->
        cannotApplyTypeToType t1 t2
  KindApp ann t1 t2 -> do
    k1 <- elaborateKind t1
    case k1 of
      ForAll _ _ a _ n _ -> do
        flip (replaceTypeVars a) n . ($> ann) <$> apply t2
      _ ->
        cannotApplyKindToType t1 t2
  ForAll ann _ _ _ _ _ -> do
    pure $ E.kindType $> ann
  ConstrainedType ann _ _ ->
    pure $ E.kindType $> ann
  KindedType ann _ k ->
    pure $ k $> ann
  ty ->
    throwError . errorMessage' (fst (getAnnForType ty)) $ UnsupportedTypeInKind ty

checkEscapedSkolems :: SourceType -> TypeCheckM ()
checkEscapedSkolems ty =
  traverse_ (throwError . toSkolemError)
    . everythingWithContextOnTypes ty [] (<>) go
    $ ty
  where
  go :: SourceType -> SourceType -> (SourceType, [(SourceSpan, Text, SourceType)])
  go ty' = \case
    Skolem ss name _ _ _ -> (ty', [(fst ss, name, ty')])
    ty''@(KindApp _ _ _) -> (ty'', [])
    _ -> (ty', [])

  toSkolemError (ss, name, ty') =
    errorMessage' (fst $ getAnnForType ty') $ EscapedSkolem name (Just ss) ty'

kindOfWithUnknowns
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM (([(Unknown, SourceType)], SourceType), SourceType)
kindOfWithUnknowns ty = do
  (ty', kind) <- kindOf ty
  unks <- unknownsWithKinds . IS.toList $ unknowns ty'
  pure ((unks, ty'), kind)

-- | Infer the kind of a single type
kindOf
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM (SourceType, SourceType)
kindOf = fmap (first snd) . kindOfWithScopedVars

-- | Infer the kind of a single type, returning the kinds of any scoped type variables
kindOfWithScopedVars
  :: (HasCallStack)
  => SourceType
  -> TypeCheckM (([(Text, SourceType)], SourceType), SourceType)
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
  -- The infered type signatures of data constructors
  , SourceType
  -- The inferred kind of the declaration
  )

kindOfData
  :: 
   ModuleName
  -> DataDeclarationArgs
  -> TypeCheckM DataDeclarationResult
kindOfData moduleName dataDecl =
  head . (^. _2) <$> kindsOfAll moduleName [] [dataDecl] []

inferDataDeclaration
  :: 
  ModuleName
  -> DataDeclarationArgs
  -> TypeCheckM [(DataConstructorDeclaration, SourceType)]
inferDataDeclaration moduleName (ann, tyName, tyArgs, ctors) = do
  tyKind <- apply =<< lookupTypeVariable moduleName (Qualified ByNullSourcePos tyName)
  let (sigBinders, tyKind') = fromJust . completeBinderList $ tyKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    tyArgs' <- for tyArgs . traverse . maybe (freshKind (fst ann)) $ replaceAllTypeSynonyms <=< apply <=< checkIsSaturatedType
    subsumesKind (foldr ((E.-:>) . snd) E.kindType tyArgs') tyKind'
    bindLocalTypeVariables moduleName (first ProperName <$> tyArgs') $ do
      let tyCtorName = srcTypeConstructor $ mkQualified tyName moduleName
          tyCtor = foldl (\ty -> srcKindApp ty . srcTypeVar . fst . snd) tyCtorName sigBinders
          tyCtor' = foldl (\ty -> srcTypeApp ty . srcTypeVar . fst) tyCtor tyArgs'
          ctorBinders = fmap (fmap (fmap Just)) $ sigBinders <> fmap (nullSourceAnn,) tyArgs'
          visibility = second (const TypeVarVisible) <$> tyArgs
      for ctors $
        fmap (fmap (addVisibility visibility . mkForAll ctorBinders)) . inferDataConstructor tyCtor'

inferDataConstructor
  :: 
   SourceType
  -> DataConstructorDeclaration
  -> TypeCheckM (DataConstructorDeclaration, SourceType)
inferDataConstructor tyCtor DataConstructorDeclaration{..} = do
  dataCtorFields' <- traverse (traverse checkIsSaturatedType) dataCtorFields
  dataCtor <- flip (foldr ((E.-:>) . snd)) dataCtorFields' <$> checkKind tyCtor E.kindType
  pure ( DataConstructorDeclaration { dataCtorFields = dataCtorFields', .. }, dataCtor )

type TypeDeclarationArgs =
  ( SourceAnn
  , ProperName 'TypeName
  , [(Text, Maybe SourceType)]
  , SourceType
  )

type TypeDeclarationResult =
  ( SourceType
  -- The elaborated rhs of the declaration
  , SourceType
  -- The inferred kind of the declaration
  )

kindOfTypeSynonym
  :: 
   ModuleName
  -> TypeDeclarationArgs
  -> TypeCheckM TypeDeclarationResult
kindOfTypeSynonym moduleName typeDecl =
  head . (^. _1) <$> kindsOfAll moduleName [typeDecl] [] []

inferTypeSynonym
  :: 
   ModuleName
  -> TypeDeclarationArgs
  -> TypeCheckM SourceType
inferTypeSynonym moduleName (ann, tyName, tyArgs, tyBody) = do
  tyKind <- apply =<< lookupTypeVariable moduleName (Qualified ByNullSourcePos tyName)
  let (sigBinders, tyKind') = fromJust . completeBinderList $ tyKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    kindRes <- freshKind (fst ann)
    tyArgs' <- for tyArgs . traverse . maybe (freshKind (fst ann)) $ replaceAllTypeSynonyms <=< apply <=< checkIsSaturatedType
    unifyKinds tyKind' $ foldr ((E.-:>) . snd) kindRes tyArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> tyArgs') $ do
      tyBodyAndKind <- traverse apply =<< inferKind tyBody
      instantiateKind tyBodyAndKind =<< apply kindRes

-- | Checks that a particular generalization is valid and well-scoped.
-- | Implicitly generalized kinds are always elaborated before explicitly
-- | quantified type variables. It's possible that such a kind can be
-- | inserted before other variables that it depends on, making it
-- | ill-scoped. We require that users explicitly generalize this kind
-- | in such a case.
checkQuantification
  :: 
  SourceType
  -> TypeCheckM ()
checkQuantification =
  collectErrors . go [] [] . fst . fromJust . completeBinderList
  where
  collectErrors vars =
    unless (null vars)
      . throwError
      . foldMap (\(ann, arg) -> errorMessage' (fst ann) $ QuantificationCheckFailureInKind arg)
      $ vars

  go acc _ [] = reverse acc
  go acc sco ((_, (arg, k)) : rest)
    | not . all (flip elem sco) $ freeTypeVariables k = goDeps acc arg rest
    | otherwise = go acc (arg : sco) rest

  goDeps acc _ [] = acc
  goDeps acc karg ((ann, (arg, k)) : rest)
    | isDep && arg == karg = (ann, arg) : acc
    | isDep = goDeps ((ann, arg) : acc) karg rest
    | otherwise = goDeps acc karg rest
    where
    isDep =
      elem karg $ freeTypeVariables k

checkVisibleTypeQuantification
  :: 
  SourceType
  -> TypeCheckM ()
checkVisibleTypeQuantification =
  collectErrors . freeTypeVariables
  where
  collectErrors vars =
    unless (null vars)
      . throwError
      . foldMap (errorMessage . VisibleQuantificationCheckFailureInType)
      $ vars

-- | Checks that there are no remaining unknowns in a type, and if so
-- | throws an error. This is necessary for contexts where we can't
-- | implicitly generalize unknowns, such as on the right-hand-side of
-- | a type synonym, or in arguments to data constructors.
checkTypeQuantification
  :: 
  SourceType
  -> TypeCheckM ()
checkTypeQuantification =
  collectErrors . everythingWithContextOnTypes True [] (<>) unknownsInKinds
  where
  collectErrors tysWithUnks =
    unless (null tysWithUnks) . throwError . foldMap toMultipleErrors $ tysWithUnks

  toMultipleErrors (ss, unks, ty) =
    errorMessage' ss $ QuantificationCheckFailureInType (IS.toList unks) ty

  unknownsInKinds False _ = (False, [])
  unknownsInKinds _ ty = case ty of
    ForAll sa _ _ _ _ _ | unks <- unknowns ty, not (IS.null unks) ->
      (False, [(fst sa, unks, ty)])
    KindApp sa _ _ | unks <- unknowns ty, not (IS.null unks) ->
      (False, [(fst sa, unks, ty)])
    ConstrainedType sa _ _ | unks <- unknowns ty, not (IS.null unks) ->
      (False, [(fst sa, unks, ty)])
    _ ->
      (True, [])

type ClassDeclarationArgs =
  ( SourceAnn
  , ProperName 'ClassName
  , [(Text, Maybe SourceType)]
  , [SourceConstraint]
  , [Declaration]
  )

type ClassDeclarationResult =
  ( [(Text, SourceType)]
  -- The kind annotated class arguments
  , [SourceConstraint]
  -- The kind annotated superclass constraints
  , [Declaration]
  -- The kind annotated declarations
  , SourceType
  -- The inferred kind of the declaration
  )

kindOfClass
  :: 
  ModuleName
  -> ClassDeclarationArgs
  -> TypeCheckM ClassDeclarationResult
kindOfClass moduleName clsDecl =
  head . (^. _3) <$> kindsOfAll moduleName [] [] [clsDecl]

inferClassDeclaration
  :: 
  ModuleName
  -> ClassDeclarationArgs
  -> TypeCheckM ([(Text, SourceType)], [SourceConstraint], [Declaration])
inferClassDeclaration moduleName (ann, clsName, clsArgs, superClasses, decls) = do
  clsKind <- apply =<< lookupTypeVariable moduleName (Qualified ByNullSourcePos $ coerceProperName clsName)
  let (sigBinders, clsKind') = fromJust . completeBinderList $ clsKind
  bindLocalTypeVariables moduleName (first ProperName . snd <$> sigBinders) $ do
    clsArgs' <- for clsArgs . traverse . maybe (freshKind (fst ann)) $ replaceAllTypeSynonyms <=< apply <=< checkIsSaturatedType
    unifyKinds clsKind' $ foldr ((E.-:>) . snd) E.kindConstraint clsArgs'
    bindLocalTypeVariables moduleName (first ProperName <$> clsArgs') $ do
      (clsArgs',,)
        <$> for superClasses checkConstraint
        <*> for decls checkClassMemberDeclaration

checkClassMemberDeclaration
  :: 
  Declaration
  -> TypeCheckM Declaration
checkClassMemberDeclaration = \case
  TypeDeclaration (TypeDeclarationData ann ident ty) ->
    TypeDeclaration . TypeDeclarationData ann ident <$> checkKind ty E.kindType
  _ -> internalError "Invalid class member declaration"

applyClassMemberDeclaration
  :: 
  Declaration
  -> TypeCheckM Declaration
applyClassMemberDeclaration = \case
  TypeDeclaration (TypeDeclarationData ann ident ty) ->
    TypeDeclaration . TypeDeclarationData ann ident <$> apply ty
  _ -> internalError "Invalid class member declaration"

mapTypeDeclaration :: (SourceType -> SourceType) -> Declaration -> Declaration
mapTypeDeclaration f = \case
  TypeDeclaration (TypeDeclarationData ann ident ty) ->
    TypeDeclaration . TypeDeclarationData ann ident $ f ty
  other ->
    other

checkConstraint
  :: 
  SourceConstraint
  -> TypeCheckM SourceConstraint
checkConstraint (Constraint ann clsName kinds args dat) = do
  let ty = foldl (TypeApp ann) (foldl (KindApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) kinds) args
  (_, kinds', args') <- unapplyTypes <$> checkKind ty E.kindConstraint
  pure $ Constraint ann clsName kinds' args' dat

applyConstraint
  :: 
  SourceConstraint
  -> TypeCheckM SourceConstraint
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
  ( [SourceConstraint]
  , [SourceType]
  , [SourceType]
  , [(Text, SourceType)]
  )

checkInstanceDeclaration
  :: 
  ModuleName
  -> InstanceDeclarationArgs
  -> TypeCheckM InstanceDeclarationResult
checkInstanceDeclaration moduleName (ann, constraints, clsName, args) = do
  let ty = foldl (TypeApp ann) (TypeConstructor ann (fmap coerceProperName clsName)) args
      tyWithConstraints = foldr srcConstrainedType ty constraints
      freeVars = freeTypeVariables tyWithConstraints
  freeVarsDict <- for freeVars $ \v -> (ProperName v,) <$> freshKind (fst ann)
  bindLocalTypeVariables moduleName freeVarsDict $ do
    ty' <- checkKind ty E.kindConstraint
    constraints' <- for constraints checkConstraint
    allTy <- apply $ foldr srcConstrainedType ty' constraints'
    allUnknowns <- unknownsWithKinds . IS.toList . foldMap unknowns . (allTy :) =<< traverse (apply . snd) freeVarsDict
    let unknownVars = unknownVarNames (usedTypeVariables allTy) allUnknowns
    let allWithVars = replaceUnknownsWithVars unknownVars allTy
    let (allConstraints, (_, allKinds, allArgs)) = unapplyTypes <$> unapplyConstraints allWithVars
    varKinds <- traverse (traverse (fmap (replaceUnknownsWithVars unknownVars) . apply)) $ (snd <$> unknownVars) <> (first runProperName <$> freeVarsDict)
    pure (allConstraints, allKinds, allArgs, varKinds)

checkKindDeclaration
  :: 
  ModuleName
  -> SourceType
  -> TypeCheckM SourceType
checkKindDeclaration _ ty = do
  (ty', kind) <- kindOf ty
  checkTypeKind kind E.kindType
  ty'' <- replaceAllTypeSynonyms ty'
  unks <- unknownsWithKinds . IS.toList $ unknowns ty''
  finalTy <- generalizeUnknowns unks <$> freshenForAlls ty' ty''
  checkQuantification finalTy
  checkValidKind finalTy
  where
  -- When expanding type synonyms and generalizing, we need to generate more
  -- unique names so that they don't clash or shadow other names, or can
  -- be referenced (easily).
  freshVar arg = (arg <>) . T.pack . show <$> fresh
  freshenForAlls = curry $ \case
    (ForAll _ _ v1 _ ty1 _, ForAll a2 vis v2 k2 ty2 sc2) | v1 == v2 -> do
      ty2' <- freshenForAlls ty1 ty2
      pure $ ForAll a2 vis v2 k2 ty2' sc2
    (_, ty2) -> go ty2 where
      go = \case
        ForAll a' vis v' k' ty' sc' -> do
          v'' <- freshVar v'
          ty'' <- go (replaceTypeVars v' (TypeVar a' v'') ty')
          pure $ ForAll a' vis v'' k' ty'' sc'
        other -> pure other

  checkValidKind = everywhereOnTypesM $ \case
    ty'@(ConstrainedType ann _ _) ->
      throwError . errorMessage' (fst ann) $ UnsupportedTypeInKind ty'
    other -> pure other

existingSignatureOrFreshKind
  :: 
  ModuleName
  -> SourceSpan
  -> ProperName 'TypeName
  -> TypeCheckM SourceType
existingSignatureOrFreshKind moduleName ss name = do
  env <- getEnv
  case M.lookup (Qualified (ByModuleName moduleName) name) (E.types env) of
    Nothing -> freshKind ss
    Just (kind, _) -> pure kind

kindsOfAll
  :: 
   ModuleName
  -> [TypeDeclarationArgs]
  -> [DataDeclarationArgs]
  -> [ClassDeclarationArgs]
  -> TypeCheckM ([TypeDeclarationResult], [DataDeclarationResult], [ClassDeclarationResult])
kindsOfAll moduleName syns dats clss = withFreshSubstitution $ do
  synDict <- for syns $ \(sa, synName, _, _) -> (synName,) <$> existingSignatureOrFreshKind moduleName (fst sa) synName
  datDict <- for dats $ \(sa, datName, _, _) -> (datName,) <$> existingSignatureOrFreshKind moduleName (fst sa) datName
  clsDict <- for clss $ \(sa, clsName, _, _, _) -> fmap (coerceProperName clsName,) $ existingSignatureOrFreshKind moduleName (fst sa) $ coerceProperName clsName
  let bindingGroup = synDict <> datDict <> clsDict
  bindLocalTypeVariables moduleName bindingGroup $ do
    synResults <- for syns (inferTypeSynonym moduleName)
    datResults <- for dats (inferDataDeclaration moduleName)
    clsResults <- for clss (inferClassDeclaration moduleName)
    synResultsWithUnks <- for (zip synDict synResults) $ \((synName, synKind), synBody) -> do
      synKind' <- apply synKind
      synBody' <- apply synBody
      pure (((synName, synKind'), synBody'), unknowns synKind')
    datResultsWithUnks <- for (zip datDict datResults) $ \((datName, datKind), ctors) -> do
      datKind' <- apply datKind
      ctors' <- traverse (bitraverse (traverseDataCtorFields (traverse (traverse apply))) apply) ctors
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
        clsResultsWithKinds = flip fmap clsResultsWithUnks $ \(((clsName, clsKind), (args, supers, decls)), _) -> do
          let tyUnks = snd . fromJust $ lookup (mkQualified clsName moduleName) tySubs
              (usedTypeVariablesInDecls, _, _, _, _) = accumTypes usedTypeVariables
              usedVars = usedTypeVariables clsKind
                      <> foldMap (usedTypeVariables . snd) args
                      <> foldMap (foldMap usedTypeVariables . (\c -> constraintKindArgs c <> constraintArgs c)) supers
                      <> foldMap usedTypeVariablesInDecls decls
              unkBinders = unknownVarNames usedVars tyUnks
              args' = fmap (replaceUnknownsWithVars unkBinders . replaceTypeCtors) <$> args
              supers' = mapConstraintArgsAll (fmap (replaceUnknownsWithVars unkBinders . replaceTypeCtors)) <$> supers
              decls' = mapTypeDeclaration (replaceUnknownsWithVars unkBinders . replaceTypeCtors) <$> decls
          (args', supers', decls', generalizeUnknownsWithVars unkBinders clsKind)
    datResultsWithKinds <- for datResultsWithUnks $ \(((datName, datKind), ctors), _) -> do
      let tyUnks = snd . fromJust $ lookup (mkQualified datName moduleName) tySubs
          replaceDataCtorField ty = replaceUnknownsWithVars (unknownVarNames (usedTypeVariables ty) tyUnks) $ replaceTypeCtors ty
          ctors' = fmap (mapDataCtorFields (fmap (fmap replaceDataCtorField)) *** generalizeUnknowns tyUnks . replaceTypeCtors) ctors
      traverse_ (traverse_ checkTypeQuantification) ctors'
      pure (ctors', generalizeUnknowns tyUnks datKind)
    synResultsWithKinds <- for synResultsWithUnks $ \(((synName, synKind), synBody), _) -> do
      let tyUnks = snd . fromJust $ lookup (mkQualified synName moduleName) tySubs
          unkBinders = unknownVarNames (usedTypeVariables synKind <> usedTypeVariables synBody) tyUnks
          genBody = replaceUnknownsWithVars unkBinders $ replaceTypeCtors synBody
          genSig = generalizeUnknownsWithVars unkBinders synKind
      checkEscapedSkolems genBody
      checkTypeQuantification genBody
      checkVisibleTypeQuantification genSig
      pure (genBody, genSig)
    pure (synResultsWithKinds, datResultsWithKinds, clsResultsWithKinds)
