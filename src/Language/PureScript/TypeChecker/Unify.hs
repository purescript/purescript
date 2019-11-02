{-# LANGUAGE FlexibleInstances #-}

-- |
-- Functions and instances relating to unification
--
module Language.PureScript.TypeChecker.Unify
  ( freshType
  , solveType
  , substituteType
  , unknownsInType
  , unifyTypes
  , unifyRows
  , alignRowsWith
  , replaceVarWithUnknown
  , replaceTypeWildcards
  , varIfUnknown
  ) where

import Prelude.Compat

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (sortBy, nubBy)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.Types

-- | Generate a fresh type variable
freshType :: (MonadState CheckState m) => m SourceType
freshType = do
  t <- gets checkNextType
  modify $ \st -> st { checkNextType = t + 1 }
  return $ srcTUnknown t

-- | Update the substitution to solve a type constraint
solveType :: (MonadError MultipleErrors m, MonadState CheckState m) => Int -> SourceType -> m ()
solveType u t = do
  occursCheck u t
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substType =
                                                    M.insert u t $ substType $ checkSubstitution cs
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
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $ unifyTypes' (substituteType sub t1) (substituteType sub t2)
  where
  unifyTypes' (TUnknown _ u1) (TUnknown _ u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown _ u) t = solveType u t
  unifyTypes' t (TUnknown _ u) = solveType u t
  unifyTypes' (ForAll ann1 ident1 _ ty1 sc1) (ForAll ann2 ident2 _ ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ann1 ident1 sko sc1' ty1
        let sk2 = skolemize ann2 ident2 sko sc2' ty2
        sk1 `unifyTypes` sk2
      _ -> internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' (ForAll ann ident _ ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ann ident sko sc ty1
    sk `unifyTypes` ty2
  unifyTypes' ForAll{} _ = internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar _ v1) (TypeVar _ v2) | v1 == v2 = return ()
  unifyTypes' ty1@(TypeConstructor _ c1) ty2@(TypeConstructor _ c2) =
    guardWith (errorMessage (TypesDoNotUnify ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = return ()
  unifyTypes' (TypeApp _ t3 t4) (TypeApp _ t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem _ _ s1 _) (Skolem _ _ s2 _) | s1 == s2 = return ()
  unifyTypes' (KindedType _ ty1 _) ty2 = ty1 `unifyTypes` ty2
  unifyTypes' ty1 (KindedType _ ty2 _) = ty1 `unifyTypes` ty2
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmpty{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmpty{} = unifyRows r1 r2
  unifyTypes' (ConstrainedType _ c1 ty1) (ConstrainedType _ c2 ty2)
    | constraintClass c1 == constraintClass c2 && constraintData c1 == constraintData c2 = do
        traverse_ (uncurry unifyTypes) (constraintArgs c1 `zip` constraintArgs c2)
        ty1 `unifyTypes` ty2
  unifyTypes' ty1@ConstrainedType{} ty2 =
    throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' t3 t4@ConstrainedType{} = unifyTypes' t4 t3
  unifyTypes' t3 t4 =
    throwError . errorMessage $ TypesDoNotUnify t3 t4

-- | Align two rows of types, splitting them into three parts:
--
-- * Those types which appear in both rows
-- * Those which appear only on the left
-- * Those which appear only on the right
--
-- Note: importantly, we preserve the order of the types with a given label.
alignRowsWith
  :: (Type a -> Type a -> r)
  -> Type a
  -> Type a
  -> ([r], (([RowListItem a], Type a), ([RowListItem a], Type a)))
alignRowsWith f ty1 ty2 = go s1 s2 where
  (s1, tail1) = rowToSortedList ty1
  (s2, tail2) = rowToSortedList ty2

  go [] r = ([], (([], tail1), (r, tail2)))
  go r [] = ([], ((r, tail1), ([], tail2)))
  go lhs@(RowListItem a1 l1 t1 : r1) rhs@(RowListItem a2 l2 t2 : r2)
    | l1 < l2 = (second . first . first) (RowListItem a1 l1 t1 :) (go r1 rhs)
    | l2 < l1 = (second . second . first) (RowListItem a2 l2 t2 :) (go lhs r2)
    | otherwise = first (f t1 t2 :) (go r1 r2)

-- | Unify two rows, updating the current substitution
--
-- Common labels are identified and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate.
unifyRows :: forall m. (MonadError MultipleErrors m, MonadState CheckState m) => SourceType -> SourceType -> m ()
unifyRows r1 r2 = sequence_ matches *> uncurry unifyTails rest where
  (matches, rest) = alignRowsWith unifyTypes r1 r2

  unifyTails :: ([RowListItem SourceAnn], SourceType) -> ([RowListItem SourceAnn], SourceType) -> m ()
  unifyTails ([], TUnknown _ u)    (sd, r)               = solveType u (rowFromList (sd, r))
  unifyTails (sd, r)               ([], TUnknown _ u)    = solveType u (rowFromList (sd, r))
  unifyTails ([], REmpty _)        ([], REmpty _)        = return ()
  unifyTails ([], TypeVar _ v1)    ([], TypeVar _ v2)    | v1 == v2 = return ()
  unifyTails ([], Skolem _ s1 _ _) ([], Skolem _ s2 _ _) | s1 == s2 = return ()
  unifyTails (sd1, TUnknown _ u1)  (sd2, TUnknown _ u2)  = do
    forM_ sd1 $ occursCheck u2 . rowListType
    forM_ sd2 $ occursCheck u1 . rowListType
    rest' <- freshType
    solveType u1 (rowFromList (sd2, rest'))
    solveType u2 (rowFromList (sd1, rest'))
  unifyTails _ _ =
    withErrorMessageHint (ErrorUnifyingTypes r1 r2) $
      throwError . errorMessage $ TypesDoNotUnify r1 r2

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: (MonadState CheckState m) => Text -> SourceType -> m SourceType
replaceVarWithUnknown ident ty = do
  tu <- freshType
  return $ replaceTypeVars ident tu ty

-- |
-- Replace type wildcards with unknowns
--
replaceTypeWildcards :: (MonadWriter MultipleErrors m, MonadState CheckState m) => SourceType -> m SourceType
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace (TypeWildcard ann name) = do
    t <- freshType
    ctx <- getLocalContext
    let err = maybe (WildcardInferredType t ctx) (\n -> HoleInferredType n t ctx Nothing) name
    warnWithPosition (fst ann) $ tell $ errorMessage err
    return t
  replace other = return other

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: SourceType -> SourceType
varIfUnknown ty =
  let unks = nubBy ((==) `on` snd) $ unknownsInType ty
      toName = T.cons 't' . T.pack .  show
      addKind a = (a, Nothing)
      ty' = everywhereOnTypes typeToVar ty
      typeToVar :: SourceType -> SourceType
      typeToVar (TUnknown ann u) = TypeVar ann (toName u)
      typeToVar t = t
  in mkForAll (fmap (fmap addKind) . sortBy (comparing snd) . fmap (fmap toName) $ unks) ty'
