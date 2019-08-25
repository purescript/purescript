module Language.PureScript.KindChecker.Kinds where

import Prelude

import Control.Monad (join, unless)
import Control.Monad.Except (throwError)
import Data.Bitraversable (bitraverse)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.IntMap.Strict as IM
import qualified Language.PureScript.Environment as E
import Language.PureScript.KindChecker.Monad
import Language.PureScript.Types

note :: MonadCheck m => KindError -> Maybe a -> m a
note err = \case
  Just a -> pure a
  Nothing -> throwError err

inferKind :: MonadCheck m => SourceType -> m (SourceType, SourceType)
inferKind ty = case ty of
  TypeConstructor ss t -> do
    n <- note (TypeNotInScope t) . fmap scType =<< lookupType t
    pure (ty, n $> ss)
  TypeLevelString ss _ ->
    pure (ty, E.kindSymbol $> ss)
  TypeVar ss a -> do
    w <- note (VarNotInScope a) . fmap scType =<< lookupVar a
    pure (ty, w $> ss)
  TUnknown ss a' -> do
    n <- note (UnknownNotInScope a') . fmap scType =<< lookupUnsolved a'
    pure (ty, n $> ss)
  TypeApp _ t1 t2 -> do
    -- TODO: use ss?
    (p1, n1) <- inferKind t1
    inferAppKind (p1, n1) t2
  KindApp ss t1 t2 -> do
    -- TODO: use ss?
    (p1, n) <- bitraverse pure apply =<< inferKind t1
    case n of
      ForAll _ a (Just w) n2 _ -> do
        p2 <- checkKind t2 w
        pure (KindApp ss p1 p2, replaceTypeVars a p2 n2)
      -- TODO: Better error for bad KindApp
      _ ->
        throwError $ InternalError "inferKind: unkinded forall binder" $ Just n
  ForAll ss a k o sk -> do
    -- TODO: use ss?
    w <- case k of
      Just k' ->
        checkKind k' E.kindType
      Nothing -> do
        a' <- unknown
        extendUnsolved Nothing a' E.kindType
        pure $ TUnknown ss a'
    (u, uns) <- scopedWithUnsolved $ do
      extendVar a w
      apply =<< checkKind o E.kindType
    unless (not . elem a $ foldMap (freeTypeVariables . scType) uns) $
      throwError $ QuantificationCheckFailure [a]
    for_ (IM.toList uns) $ \(b', ScopeValue _ k' _) ->
      extendUnsolved Nothing b' k'
    pure (ForAll ss a (Just w) u sk, E.kindType $> ss)
  KindedType _ t1 t2 -> do
    p1 <- checkKind t1 t2
    p2 <- apply t2
    pure (p1, p2)
  TypeWildcard ss _ -> do
    u <- unknown
    extendUnsolved Nothing u E.kindType
    pure (ty, TUnknown ss u)
  REmpty ss -> do
    u <- unknown
    extendUnsolved Nothing u E.kindType
    pure (ty, E.kindRow (TUnknown ss u) $> ss)
  RCons ss lbl t1 t2 -> do
    (p1, w1) <- inferKind t1
    (p2, w2) <- inferKind t2
    unify w2 (E.kindRow w1)
    -- TODO: Do we need to apply p1?
    pure (RCons ss lbl p1 p2, E.kindRow w1 $> ss)
  _ ->
    throwError $ InternalError "inferKind: Unimplemented case" $ Just ty

inferAppKind :: MonadCheck m => (SourceType, SourceType) -> SourceType -> m (SourceType, SourceType)
inferAppKind (p1, knd) t = case knd of
  TypeApp ss1 (TypeApp _ k w1) w2 | eqType k E.tyFunction -> do
    p2 <- checkKind t w1
    pure (TypeApp ss1 p1 p2, w2)
  TypeApp ss1 (TypeApp _ k w1) w2 | eqType k E.tyConstrainedValue -> do
    p2 <- checkKind t w1
    pure (TypeApp ss1 p1 p2, w2)
  TUnknown ss a' -> do
    ScopeValue lvl w _ <- note (UnknownNotInScope a') =<< lookupUnsolved a'
    a1' <- unknown
    a2' <- unknown
    extendUnsolved (Just lvl) a1' E.kindType
    extendUnsolved (Just lvl) a2' E.kindType
    solve a' w $ (TUnknown ss a1' E.-:> TUnknown ss a2') $> ss
    p2 <- checkKind t $ TUnknown ss a1'
    pure (TypeApp ss p1 p2, TUnknown ss a2')
  ForAll ss a (Just w1) n _ -> do
    a' <- unknown
    extendUnsolved Nothing a' w1
    inferAppKind (KindApp ss p1 (TUnknown ss a'), replaceTypeVars a (TUnknown ss a') n) t
  -- TODO: Better error
  _ ->
    throwError $ CannotApplyType (p1, knd) t

checkKind :: MonadCheck m => SourceType -> SourceType -> m SourceType
checkKind o w = do
  (u1, n) <- inferKind o
  n_ <- apply n
  w_ <- apply w
  instantiate (u1, n_) w_

instantiate :: MonadCheck m => (SourceType, SourceType) -> SourceType -> m SourceType
instantiate (u1, k) w2 = case k of
  ForAll ss a (Just w1) n _ -> do
    a' <- unknown
    extendUnsolved Nothing a' w1
    instantiate (KindApp ss u1 (TUnknown ss a'), replaceTypeVars a (TUnknown ss a') n) w2
  -- TODO: Should we have an error for constraint instantiation?
  w1 -> do
    unify w1 w2
    pure u1

unify :: MonadCheck m => SourceType -> SourceType -> m ()
unify = curry $ \case
  (TypeApp _ p1 p2, TypeApp _ p3 p4) -> do
    unify p1 p3
    join $ unify <$> apply p2 <*> apply p4
  (KindApp _ p1 p2, KindApp _ p3 p4) -> do
    unify p1 p3
    join $ unify <$> apply p2 <*> apply p4
  (w1, w2) | eqType w1 w2 ->
    pure ()
  (TUnknown ss a', p1) ->
    unifyUnknown ss a' p1
  (p1, TUnknown ss a') ->
    unifyUnknown ss a' p1
  (w1, w2) ->
    throwError $ DoesNotUnify w1 w2
  where
  unifyUnknown _ a' p1 = do
    p2 <- promote a' p1
    w1 <- note (UnknownNotInScope a') . fmap scType =<< lookupUnsolved a'
    join $ unify <$> apply w1 <*> elaboratedKind p2
    solve a' w1 p2

promote :: MonadCheck m => Int -> SourceType -> m SourceType
promote a' ty = case ty of
  TypeLevelString _ _ ->
    pure ty
  TypeConstructor _ t -> do
    ScopeValue lvl1 _ _ <- note (TypeNotInScope t) =<< lookupType t
    ScopeValue lvl2 _ _ <- note (UnknownNotInScope a') =<< lookupUnsolved a'
    unless (lvl1 < lvl2) . throwError $ TypeNotInScope t
    pure ty
  TypeVar _ a -> do
    ScopeValue lvl1 _ _ <- note (VarNotInScope a) =<< lookupVar a
    ScopeValue lvl2 _ _ <- note (UnknownNotInScope a') =<< lookupUnsolved a'
    unless (lvl1 < lvl2) . throwError $ VarNotInScope a
    pure ty
  TypeApp ss w1 w2 -> do
    p1 <- promote a' w1
    p2 <- promote a' =<< apply w2
    pure $ TypeApp ss p1 p2
  KindApp ss w1 w2 -> do
    p1 <- promote a' w1
    p2 <- promote a' =<< apply w2
    pure $ KindApp ss p1 p2
  TUnknown _ b' | a' == b' ->
    throwError $ InfiniteKind ty
  TUnknown ss b' -> do
    ScopeValue lvl1 p _ <- note (UnknownNotInScope b') =<< lookupUnsolved b'
    ScopeValue lvl2 _ _ <- note (UnknownNotInScope a') =<< lookupUnsolved a'
    if lvl1 < lvl2 then
      pure ty
    else do
      p1  <- promote a' =<< apply p
      b1' <- unknown
      extendUnsolved (Just lvl2) b1' p1
      solve b' p $ TUnknown ss b1'
      pure $ TUnknown ss b1'
  _ ->
    throwError $ InternalError "promote: Unimplemented case" $ Just ty

elaboratedKind :: MonadCheck m => SourceType -> m SourceType
elaboratedKind = \case
  TypeLevelString ss _ ->
    pure $ E.kindType $> ss
  TypeConstructor ss t -> do
    n <- note (TypeNotInScope t) . fmap scType =<< lookupType t
    ($> ss) <$> apply n
  TypeVar ss a -> do
    w <- note (VarNotInScope a) . fmap scType =<< lookupVar a
    ($> ss) <$> apply w
  TUnknown ss a' -> do
    w <- note (UnknownNotInScope a') . fmap scType =<< lookupUnsolved a'
    ($> ss) <$> apply w
  TypeApp ss p1 p2 -> do
    p1Kind <- elaboratedKind p1
    case p1Kind of
      TypeApp _ (TypeApp _ k w1) w2 | eqType k E.tyFunction -> do
        p2Kind <- elaboratedKind p2
        unless (eqType p2Kind w1) $
          throwError $ CannotApplyType (p1, p2) p2Kind
        pure $ w2 $> ss
      _ ->
        throwError $ CannotApplyType (p1, p2) p1Kind
  KindApp ss p1 p2 -> do
    p1Kind <- elaboratedKind p1
    case p1Kind of
      ForAll _ a (Just w) n _ -> do
        p2Kind <- elaboratedKind p2
        unless (eqType p2Kind w) $
          throwError $ CannotApplyKind (p1, p2) p2Kind
        flip (replaceTypeVars a) n . ($> ss) <$> apply p2
      _ ->
        throwError $ CannotApplyKind (p1, p2) p1
  ForAll ss a (Just w) u _ -> do
    wIsStar <- elaboratedKind w
    unless (eqType wIsStar E.kindType) $
      throwError $ ElaboratedKindIsNotType w
    uIsStar <- scoped $ do
      extendVar a w
      elaboratedKind u
    unless (eqType uIsStar E.kindType) $
      throwError $ ElaboratedKindIsNotType w
    pure $ E.kindType $> ss
  ty ->
    throwError $ InternalError "elaboratedKind: Unimplemented case" $ Just ty

