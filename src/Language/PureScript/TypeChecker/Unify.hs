-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Unify
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Functions and instances relating to unification
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.PureScript.TypeChecker.Unify (
    unifyTypes,
    unifyRows,
    unifiesWith,
    replaceVarWithUnknown,
    replaceTypeWildcards,
    varIfUnknown
) where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as H

import Control.Monad
import Control.Monad.Unify
import Control.Monad.Writer
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Types

instance Partial Type where
  unknown = TUnknown
  isUnknown (TUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns = everythingOnTypes (++) go
    where
    go (TUnknown u) = [u]
    go _ = []
  ($?) sub = everywhereOnTypes go
    where
    go t@(TUnknown u) = fromMaybe t $ H.lookup u (runSubstitution sub)
    go other = other

instance Unifiable Check Type where
  (=?=) = unifyTypes

-- |
-- Unify two types, updating the current substitution
--
unifyTypes :: Type -> Type -> UnifyT Type Check ()
unifyTypes t1 t2 = rethrow (onErrorMessages (ErrorUnifyingTypes t1 t2)) $
  unifyTypes' t1 t2
  where
  unifyTypes' (TUnknown u1) (TUnknown u2) | u1 == u2 = return ()
  unifyTypes' (TUnknown u) t = u =:= t
  unifyTypes' t (TUnknown u) = u =:= t
  unifyTypes' (SaturatedTypeSynonym name args) ty = do
    ty1 <- introduceSkolemScope <=< expandTypeSynonym name $ args
    ty1 `unifyTypes` ty
  unifyTypes' ty s@(SaturatedTypeSynonym _ _) = s `unifyTypes` ty
  unifyTypes' (ForAll ident1 ty1 sc1) (ForAll ident2 ty2 sc2) =
    case (sc1, sc2) of
      (Just sc1', Just sc2') -> do
        sko <- newSkolemConstant
        let sk1 = skolemize ident1 sko sc1' ty1
        let sk2 = skolemize ident2 sko sc2' ty2
        sk1 `unifyTypes` sk2
      _ -> error "Skolemized type variable was not given a scope"
  unifyTypes' (ForAll ident ty1 (Just sc)) ty2 = do
    sko <- newSkolemConstant
    let sk = skolemize ident sko sc ty1
    sk `unifyTypes` ty2
  unifyTypes' ForAll{} _ = throwError . errorMessage $ UnspecifiedSkolemScope
  unifyTypes' ty f@ForAll{} = f `unifyTypes` ty
  unifyTypes' (TypeVar v1) (TypeVar v2) | v1 == v2 = return ()
  unifyTypes' ty1@(TypeConstructor c1) ty2@(TypeConstructor c2) =
    guardWith (errorMessage (TypesDoNotUnify ty1 ty2)) (c1 == c2)
  unifyTypes' (TypeApp t3 t4) (TypeApp t5 t6) = do
    t3 `unifyTypes` t5
    t4 `unifyTypes` t6
  unifyTypes' (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = return ()
  unifyTypes' (KindedType ty1 _) ty2 = ty1 `unifyTypes` ty2
  unifyTypes' ty1 (KindedType ty2 _) = ty1 `unifyTypes` ty2
  unifyTypes' r1@RCons{} r2 = unifyRows r1 r2
  unifyTypes' r1 r2@RCons{} = unifyRows r1 r2
  unifyTypes' r1@REmpty r2 = unifyRows r1 r2
  unifyTypes' r1 r2@REmpty = unifyRows r1 r2
  unifyTypes' ty1@(ConstrainedType _ _) ty2 = throwError . errorMessage $ ConstrainedTypeUnified ty1 ty2
  unifyTypes' t3 t4@(ConstrainedType _ _) = unifyTypes' t4 t3
  unifyTypes' t3 t4 = throwError . errorMessage $ TypesDoNotUnify t3 t4

-- |
-- Unify two rows, updating the current substitution
--
-- Common labels are first identified, and unified. Remaining labels and types are unified with a
-- trailing row unification variable, if appropriate, otherwise leftover labels result in a unification
-- error.
--
unifyRows :: Type -> Type -> UnifyT Type Check ()
unifyRows r1 r2 =
  let
    (s1, r1') = rowToList r1
    (s2, r2') = rowToList r2
    int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
    sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
    sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in do
    forM_ int (uncurry (=?=))
    unifyRows' sd1 r1' sd2 r2'
  where
  unifyRows' :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> UnifyT Type Check ()
  unifyRows' [] (TUnknown u) sd r = u =:= rowFromList (sd, r)
  unifyRows' sd r [] (TUnknown u) = u =:= rowFromList (sd, r)
  unifyRows' sd1 (TUnknown u1) sd2 (TUnknown u2) = do
    forM_ sd1 $ \(_, t) -> occursCheck u2 t
    forM_ sd2 $ \(_, t) -> occursCheck u1 t
    rest <- fresh
    u1 =:= rowFromList (sd2, rest)
    u2 =:= rowFromList (sd1, rest)
  unifyRows' sd1 (SaturatedTypeSynonym name args) sd2 r2' = do
    r1' <- expandTypeSynonym name $ args
    unifyRows (rowFromList (sd1, r1')) (rowFromList (sd2, r2'))
  unifyRows' sd1 r1' sd2 r2'@(SaturatedTypeSynonym _ _) = unifyRows' sd2 r2' sd1 r1'
  unifyRows' [] REmpty [] REmpty = return ()
  unifyRows' [] (TypeVar v1) [] (TypeVar v2) | v1 == v2 = return ()
  unifyRows' [] (Skolem _ s1 _) [] (Skolem _ s2 _) | s1 == s2 = return ()
  unifyRows' sd3 r3 sd4 r4 = throwError . errorMessage $ TypesDoNotUnify (rowFromList (sd3, r3)) (rowFromList (sd4, r4))

-- |
-- Check that two types unify
--
unifiesWith :: Environment -> Type -> Type -> Bool
unifiesWith _ (TUnknown u1) (TUnknown u2) | u1 == u2 = True
unifiesWith _ (Skolem _ s1 _) (Skolem _ s2 _) | s1 == s2 = True
unifiesWith _ (TypeVar v1) (TypeVar v2) | v1 == v2 = True
unifiesWith _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = True
unifiesWith e (TypeApp h1 t1) (TypeApp h2 t2) = unifiesWith e h1 h2 && unifiesWith e t1 t2
unifiesWith e (SaturatedTypeSynonym name args) t2 =
  case expandTypeSynonym' e name args of
    Left  _  -> False
    Right t1 -> unifiesWith e t1 t2
unifiesWith e t1 t2@(SaturatedTypeSynonym _ _) = unifiesWith e t2 t1
unifiesWith _ REmpty REmpty = True
unifiesWith e r1@(RCons _ _ _) r2@(RCons _ _ _) =
  let (s1, r1') = rowToList r1
      (s2, r2') = rowToList r2

      int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
      sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
      sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in all (\(t1, t2) -> unifiesWith e t1 t2) int && go sd1 r1' sd2 r2'
  where
  go :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> Bool
  go [] REmpty          [] REmpty          = True
  go [] (TypeVar v1)    [] (TypeVar v2)    = v1 == v2
  go [] (Skolem _ s1 _) [] (Skolem _ s2 _) = s1 == s2
  go [] (TUnknown _)    _  _               = True
  go _  _               [] (TUnknown _)    = True
  go _  (TUnknown _)    _  (TUnknown _)    = True
  go _  _               _  _               = False
unifiesWith _ _ _ = False

-- |
-- Replace a single type variable with a new unification variable
--
replaceVarWithUnknown :: String -> Type -> UnifyT Type Check Type
replaceVarWithUnknown ident ty = do
  tu <- fresh
  return $ replaceTypeVars ident tu ty

-- |
-- Replace type wildcards with unknowns
--
replaceTypeWildcards :: Type -> UnifyT t Check Type
replaceTypeWildcards = everywhereOnTypesM replace
  where
  replace TypeWildcard = do
    u <- fresh'
    liftCheck . tell $ errorMessage . WildcardInferredType $ TUnknown u
    return $ TUnknown u
  replace other = return other

-- |
-- Replace outermost unsolved unification variables with named type variables
--
varIfUnknown :: Type -> Type
varIfUnknown ty =
  let unks = nub $ unknowns ty
      toName = (:) 't' . show
      ty' = everywhereOnTypes typeToVar ty
      typeToVar :: Type -> Type
      typeToVar (TUnknown u) = TypeVar (toName u)
      typeToVar t = t
  in mkForAll (sort . map toName $ unks) ty'
