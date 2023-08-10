-- |
-- Functions and instances relating to unification
--
module Language.PureScript.TypeChecker.Unify
  ( freshType
  , freshTypeWithKind
  , solveType
  , substituteType
  , unknownsInType
  , unifyTypes
  , unifyTypesOrdered
  , unifyRows
  , unifyishRowTypes
  , alignRowsWith
  , replaceTypeWildcards
  , varIfUnknown
  ) where

import Prelude

import Control.Monad (forM_, void)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify, state)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Text qualified as T

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as E
import Language.PureScript.Errors (ErrorMessageHint(..), MultipleErrors(..), SimpleErrorMessage(..), SourceAnn, errorMessage, internalCompilerError, onErrorMessages, rethrow, warnWithPosition, withoutPosition)
import Language.PureScript.TypeChecker.Kinds (elaborateKind, instantiateKind, unifyKinds')
import Language.PureScript.TypeChecker.Monad (CheckState(..), Substitution(..), UnkLevel(..), Unknown, getLocalContext, guardWith, lookupUnkName, withErrorMessageHint)
import Language.PureScript.TypeChecker.Skolems (newSkolemConstant, skolemize)
import Language.PureScript.TypeChecker.Unify.Rows (unifyishRows, isTypesDoNotUnify)
import Language.PureScript.Types (Constraint(..), pattern REmptyKinded, RowListItem(..), SourceType, Type(..), WildcardData(..), alignRowsWith, everythingOnTypes, everywhereOnTypes, everywhereOnTypesM, getAnnForType, mkForAll, rowFromList, srcTUnknown)

-- | Generate a fresh type variable with an unknown kind. Avoid this if at all possible.
freshType :: (MonadState CheckState m) => m SourceType
freshType = state $ \st -> do
  let
    t = checkNextType st
    st' = st { checkNextType = t + 2
             , checkSubstitution =
                 (checkSubstitution st) { substUnsolved = M.insert t (UnkLevel (pure t), E.kindType)
                                                        . M.insert (t + 1) (UnkLevel (pure (t + 1)), srcTUnknown t)
                                                        . substUnsolved
                                                        $ checkSubstitution st
                                        }
             }
  (srcTUnknown (t + 1), st')

-- | Generate a fresh type variable with a known kind.
freshTypeWithKind :: (MonadState CheckState m) => SourceType -> m SourceType
freshTypeWithKind kind = state $ \st -> do
  let
    t = checkNextType st
    st' = st { checkNextType = t + 1
             , checkSubstitution =
                 (checkSubstitution st) { substUnsolved = M.insert t (UnkLevel (pure t), kind) (substUnsolved (checkSubstitution st)) }
             }
  (srcTUnknown t, st')

-- | Update the substitution to solve a type constraint
solveType :: (MonadError MultipleErrors m, MonadState CheckState m) => Int -> SourceType -> m ()
solveType u t = rethrow (onErrorMessages withoutPosition) $ do
  -- We strip the position so that any errors get rethrown with the position of
  -- the original unification constraint. Otherwise errors may arise from arbitrary
  -- locations. We don't otherwise have the "correct" position on hand, since it
  -- is maintained as part of the type-checker stack.
  occursCheck u t
  k1 <- elaborateKind t
  subst <- gets checkSubstitution
  k2 <- maybe (internalCompilerError ("No kind for unification variable ?" <> T.pack (show u))) (pure . substituteType subst . snd) . M.lookup u . substUnsolved $ subst
  t' <- instantiateKind (t, k1) k2
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substType =
                                                    M.insert u t' $ substType $ checkSubstitution cs
                                                }
                     }

-- | Apply a substitution to a type
substituteType :: Substitution -> SourceType -> SourceType
substituteType sub = everywhereOnTypes go
  where
  go (TUnknown ann u) =
    case M.lookup u (substType sub) of
      Nothing -> TUnknown ann u
      Just (TUnknown ann' u1) | u1 == u -> TUnknown ann' u1
      Just t -> substituteType sub t
  go other = other

-- | Make sure that an unknown does not occur in a type
occursCheck :: (MonadError MultipleErrors m) => Int -> SourceType -> m ()
occursCheck _ TUnknown{} = return ()
occursCheck u t = void $ everywhereOnTypesM go t
  where
  go (TUnknown _ u') | u == u' = throwError . errorMessage . InfiniteType $ t
  go other = return other

-- | Compute a list of all unknowns appearing in a type
unknownsInType :: Type a -> [(a, Int)]
unknownsInType t = everythingOnTypes (.) go t []
  where
  go :: Type a -> [(a, Int)] -> [(a, Int)]
  go (TUnknown ann u) = ((ann, u) :)
  go _ = id

-- | Unify two types, updating the current substitution
unifyTypes :: (MonadError MultipleErrors m, MonadState CheckState m) => SourceType -> SourceType -> m ()
unifyTypes = unifyTypesGeneral False

-- | Unify two types (in a context where the second type is expected to subsume
-- the first), updating the current substitution
unifyTypesOrdered :: (MonadError MultipleErrors m, MonadState CheckState m) => SourceType -> SourceType -> m ()
unifyTypesOrdered = unifyTypesGeneral True

unifyTypesGeneral :: (MonadError MultipleErrors m, MonadState CheckState m) => Bool -> SourceType -> SourceType -> m ()
unifyTypesGeneral isOrdered t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $ unifyTypes' (substituteType sub t1) (substituteType sub t2)
  where
  unifyTypes' (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown _ u) t = solveType u t
  unifyTypes' t (TUnknown _ u) = solveType u t
  unifyTypes' (ForAll ann1 _ ident1 mbK1 ty1 sc1) (ForAll ann2 _ ident2 mbK2 ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ann1 ident1 mbK1 sko sc1' ty1
        let sk2 = skolemize ann2 ident2 mbK2 sko sc2' ty2
        sk1 `unifyTypesCovariant` sk2
      _ -> internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' (ForAll ann _ ident mbK ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ann ident mbK sko sc ty1
    sk `unifyTypesCovariant` ty2
  unifyTypes' ForAll{} _ = internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = return ()
  unifyTypes' ty1@(TypeConstructor _ c1) ty2@(TypeConstructor _ c2) =
    guardWith (errorMessage (TypesDoNotUnify isOrdered ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = return ()
  unifyTypes' (TypeLevelInt    _ n1) (TypeLevelInt    _ n2) | n1 == n2 = return ()
  unifyTypes' (TypeApp _ t3 t4) (TypeApp _ t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (KindApp _ t3 t4) (KindApp _ t5 t6) = do
    t3 `unifyKinds'` t5
    t4 `unifyTypesCovariant` t6
  unifyTypes' (Skolem _ _ _ s1 _) (Skolem _ _ _ s2 _) | s1 == s2 = return ()
  unifyTypes' (KindedType _ ty1 _) ty2 = ty1 `unifyTypesCovariant` ty2
  unifyTypes' ty1 (KindedType _ ty2 _) = ty1 `unifyTypesCovariant` ty2
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmptyKinded{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmptyKinded{} = unifyRows r1 r2
  unifyTypes' (ConstrainedType _ c1 ty1) (ConstrainedType _ c2 ty2)
    | constraintClass c1 == constraintClass c2 && constraintData c1 == constraintData c2 = do
        traverse_ (uncurry unifyTypes) (constraintArgs c1 `zip` constraintArgs c2)
        ty1 `unifyTypesCovariant` ty2
  unifyTypes' ty1@ConstrainedType{} ty2 =
    throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' ty1 ty2@ConstrainedType{} =
    throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' t3 t4 =
    throwError . errorMessage $ TypesDoNotUnify isOrdered t3 t4

  unifyTypesCovariant = unifyTypesGeneral isOrdered

-- | Unify two rows, updating the current substitution
--
-- Common labels are identified and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate.
unifyRows :: forall m. (MonadError MultipleErrors m, MonadState CheckState m) => SourceType -> SourceType -> m ()
unifyRows = unifyishRowTypes False id id unifyTypes

-- | Sits between unifyRows and unifyishRows in generality. These names are
-- getting ridiculous.
unifyishRowTypes
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => Bool -- ^ is the first type being used where the second type is expected
  -> (SourceType -> SourceType) -- ^ how to wrap the first type when reporting an overall type error
  -> (SourceType -> SourceType) -- ^ ditto the second type
  -> (SourceType -> SourceType -> m ()) -- ^ function to use to check types at common labels
  -> SourceType
  -> SourceType
  -> m ()
unifyishRowTypes isOrdered errorTypeWrapper1 errorTypeWrapper2 =
  unifyishRows (uncurry unifyTails) isTypesDoNotUnify $ \r1 r2 -> do
    subst <- gets checkSubstitution
    pure $ errorMessage $ TypesDoNotUnify isOrdered
      (errorTypeWrapper1 $ substituteType subst r1)
      (errorTypeWrapper2 $ substituteType subst r2)
  where
  unifyTails :: ([RowListItem SourceAnn], SourceType) -> ([RowListItem SourceAnn], SourceType) -> m Bool
  unifyTails ([], TUnknown _ u)      (sd, r)                            = solveType u (rowFromList (sd, r)) $> True
  unifyTails (sd, r)                 ([], TUnknown _ u)                 = solveType u (rowFromList (sd, r)) $> True
  unifyTails ([], REmptyKinded _ _)  ([], REmptyKinded _ _)             = pure True
  unifyTails ([], TypeVar _ v1)      ([], TypeVar _ v2)      | v1 == v2 = pure True
  unifyTails ([], Skolem _ _ _ s1 _) ([], Skolem _ _ _ s2 _) | s1 == s2 = pure True
  unifyTails (sd1, TUnknown a u1)    (sd2, TUnknown _ u2)    | u1 /= u2 = do
    forM_ sd1 $ occursCheck u2 . rowListType
    forM_ sd2 $ occursCheck u1 . rowListType
    rest' <- freshTypeWithKind =<< elaborateKind (TUnknown a u1)
    solveType u1 (rowFromList (sd2, rest'))
    solveType u2 (rowFromList (sd1, rest'))
    pure True
  unifyTails _ _ = pure False

-- |
-- Replace type wildcards with unknowns
--
replaceTypeWildcards :: (MonadWriter MultipleErrors m, MonadState CheckState m) => SourceType -> m SourceType
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace (TypeWildcard ann wdata) = do
    t <- freshType
    ctx <- getLocalContext
    let err = case wdata of
          HoleWildcard n -> Just $ HoleInferredType n t ctx Nothing
          UnnamedWildcard -> Just $ WildcardInferredType t ctx
          IgnoredWildcard -> Nothing
    forM_ err $ warnWithPosition (fst ann) . tell . errorMessage
    return t
  replace other = return other

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: forall m. (MonadState CheckState m) => [(Unknown, SourceType)] -> SourceType -> m SourceType
varIfUnknown unks ty = do
  bn' <- traverse toBinding unks
  ty' <- go ty
  pure $ mkForAll bn' ty'
  where
  toName :: Unknown -> m T.Text
  toName u = (<> T.pack (show u)) . fromMaybe "t" <$> lookupUnkName u

  toBinding :: (Unknown, SourceType) -> m (SourceAnn, (T.Text, Maybe SourceType))
  toBinding (u, k) = do
    u' <- toName u
    k' <- go k
    pure (getAnnForType ty, (u', Just k'))

  go :: SourceType -> m SourceType
  go = everywhereOnTypesM $ \case
    (TUnknown ann u) ->
      TypeVar ann <$> toName u
    t -> pure t
