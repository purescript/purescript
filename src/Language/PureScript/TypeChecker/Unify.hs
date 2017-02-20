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
  , replaceVarWithUnknown
  , replaceTypeWildcards
  , varIfUnknown
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.List (sort)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.Label (Label(..))
import Language.PureScript.Types

-- | Generate a fresh type variable
freshType :: (MonadState CheckState m) => m Type
freshType = do
  t <- gets checkNextType
  modify $ \st -> st { checkNextType = t + 1 }
  return $ TUnknown t

-- | Update the substitution to solve a type constraint
solveType :: (MonadError MultipleErrors m, MonadState CheckState m) => Int -> Type -> m ()
solveType u t = do
  occursCheck u t
  modify $ \cs -> cs { checkSubstitution =
                         (checkSubstitution cs) { substType =
                                                    M.insert u t $ substType $ checkSubstitution cs
                                                }
                     }

-- | Apply a substitution to a type
substituteType :: Substitution -> Type -> Type
substituteType sub = everywhereOnTypes go
  where
  go (TUnknown u) =
    case M.lookup u (substType sub) of
      Nothing -> TUnknown u
      Just (TUnknown u1) | u1 == u -> TUnknown u1
      Just t -> substituteType sub t
  go other = other

-- | Make sure that an unknown does not occur in a type
occursCheck :: (MonadError MultipleErrors m) => Int -> Type -> m ()
occursCheck _ TUnknown{} = return ()
occursCheck u t = void $ everywhereOnTypesM go t
  where
  go (TUnknown u') | u == u' = throwError . errorMessage . InfiniteType $ t
  go other = return other

-- | Compute a list of all unknowns appearing in a type
unknownsInType :: Type -> [Int]
unknownsInType t = everythingOnTypes (.) go t []
  where
  go :: Type -> [Int] -> [Int]
  go (TUnknown u) = (u :)
  go _ = id

-- | Unify two types, updating the current substitution
unifyTypes :: (MonadError MultipleErrors m, MonadState CheckState m) => Type -> Type -> m ()
unifyTypes t1 t2 = do
  sub <- gets checkSubstitution
  withErrorMessageHint (ErrorUnifyingTypes t1 t2) $ unifyTypes' (substituteType sub t1) (substituteType sub t2)
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown u) t = solveType u t
  unifyTypes' t (TUnknown u) = solveType u t
  unifyTypes' (ForAll ident1 ty1 sc1) (ForAll ident2 ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ident1 sko sc1' Nothing ty1
        let sk2 = skolemize ident2 sko sc2' Nothing ty2
        sk1 `unifyTypes` sk2
      _ -> internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' (ForAll ident ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ident sko sc Nothing ty1
    sk `unifyTypes` ty2
  unifyTypes' ForAll{} _ = internalError "unifyTypes: unspecified skolem scope"
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar v1) (TypeVar v2) | v1 == v2 = return ()
  unifyTypes' ty1@(TypeConstructor c1) ty2@(TypeConstructor c2) =
    guardWith (errorMessage (TypesDoNotUnify ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeLevelString s1) (TypeLevelString s2) | s1 == s2 = return ()
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem _ s1 _ _) (Skolem _ s2 _ _) | s1 == s2 = return ()
  unifyTypes' (KindedType ty1 _) ty2 = ty1 `unifyTypes` ty2
  unifyTypes' ty1 (KindedType ty2 _) = ty1 `unifyTypes` ty2
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmpty r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmpty = unifyRows r1 r2
  unifyTypes' ty1@(ConstrainedType _ _) ty2 =
    throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' t3 t4@(ConstrainedType _ _) = unifyTypes' t4 t3
  unifyTypes' t3 t4 =
    throwError . errorMessage $ TypesDoNotUnify t3 t4

-- |
-- Unify two rows, updating the current substitution
--
-- Common labels are first identified, and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate, otherwise leftover labels result in a unification
-- error.
--
unifyRows :: forall m. (MonadError MultipleErrors m, MonadState CheckState m) => Type -> Type -> m ()
unifyRows r1 r2 =
  let
    (s1, r1') = rowToList r1
    (s2, r2') = rowToList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
    sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in do
    forM_ int (uncurry unifyTypes)
    unifyRows' sd1 r1' sd2 r2'
  where
  unifyRows' :: [(Label, Type)] -> Type -> [(Label, Type)] -> Type -> m ()
  unifyRows' [] (TUnknown u) sd r = solveType u (rowFromList (sd, r))
  unifyRows' sd r [] (TUnknown u) = solveType u (rowFromList (sd, r))
  unifyRows' sd1 (TUnknown u1) sd2 (TUnknown u2) = do
    forM_ sd1 $ \(_, t) -> occursCheck u2 t
    forM_ sd2 $ \(_, t) -> occursCheck u1 t
    rest <- freshType
    solveType u1 (rowFromList (sd2, rest))
    solveType u2 (rowFromList (sd1, rest))
  unifyRows' [] REmpty [] REmpty = return ()
  unifyRows' [] (TypeVar v1) [] (TypeVar v2) | v1 == v2 = return ()
  unifyRows' [] (Skolem _ s1 _ _) [] (Skolem _ s2 _ _) | s1 == s2 = return ()
  unifyRows' _ _ _ _ =
    throwError . errorMessage $ TypesDoNotUnify r1 r2

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: (MonadState CheckState m) => Text -> Type -> m Type
replaceVarWithUnknown ident ty = do
  tu <- freshType
  return $ replaceTypeVars ident tu ty

-- |
-- Replace type wildcards with unknowns
--
replaceTypeWildcards :: (MonadWriter MultipleErrors m, MonadState CheckState m) => Type -> m Type
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace (TypeWildcard ss) = do
    t <- freshType
    ctx <- getLocalContext
    warnWithPosition ss $ tell . errorMessage $ WildcardInferredType t ctx
    return t
  replace other = return other

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: Type -> Type
varIfUnknown ty =
  let unks = ordNub $ unknownsInType ty
      toName = T.cons 't' . T.pack .  show
      ty' = everywhereOnTypes typeToVar ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown u) = TypeVar (toName u)
      typeToVar t = t
  in mkForAll (sort . map toName $ unks) ty'
