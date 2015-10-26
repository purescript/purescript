-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Kinds
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the kind checker
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.TypeChecker.Kinds (
    kindOf,
    kindOfWithScopedVars,
    kindsOf,
    kindsOfAll
) where

import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as H
import qualified Data.Map as M

import Control.Arrow (second)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State
import Control.Monad.Unify

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

instance Partial Kind where
  unknown = KUnknown
  isUnknown (KUnknown u) = Just u
  isUnknown _ = Nothing
  unknowns = everythingOnKinds (++) go
    where
    go (KUnknown u) = [u]
    go _ = []
  ($?) sub = everywhereOnKinds go
    where
    go t@(KUnknown u) = fromMaybe t $ H.lookup u (runSubstitution sub)
    go other = other

instance Unifiable Check Kind where
  KUnknown u1 =?= KUnknown u2 | u1 == u2 = return ()
  KUnknown u =?= k = u =:= k
  k =?= KUnknown u = u =:= k
  Star =?= Star = return ()
  Bang =?= Bang = return ()
  Row k1 =?= Row k2 = k1 =?= k2
  FunKind k1 k2 =?= FunKind k3 k4 = do
    k1 =?= k3
    k2 =?= k4
  k1 =?= k2 = UnifyT . lift . throwError . errorMessage $ KindsDoNotUnify k1 k2

-- |
-- Infer the kind of a single type
--
kindOf :: Type -> Check Kind
kindOf ty = fst <$> kindOfWithScopedVars ty

-- |
-- Infer the kind of a single type, returning the kinds of any scoped type variables
--
kindOfWithScopedVars :: Type -> Check (Kind, [(String, Kind)])
kindOfWithScopedVars ty =
  rethrow (addHint (ErrorCheckingKind ty)) $
    fmap tidyUp . liftUnify $ infer ty
  where
  tidyUp ((k, args), sub) = ( starIfUnknown (sub $? k)
                            , map (second (starIfUnknown . (sub $?))) args
                            )

-- |
-- Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
--
kindsOf :: Bool -> ModuleName -> ProperName -> [(String, Maybe Kind)] -> [Type] -> Check Kind
kindsOf isData moduleName name args ts = fmap tidyUp . liftUnify $ do
  tyCon <- fresh
  kargs <- replicateM (length args) fresh
  rest <- zipWithM freshKindVar args kargs
  let dict = (name, tyCon) : rest
  bindLocalTypeVariables moduleName dict $
    solveTypes isData ts kargs tyCon
  where
  tidyUp (k, sub) = starIfUnknown $ sub $? k

freshKindVar :: (String, Maybe Kind) -> Kind -> UnifyT Kind Check (ProperName, Kind)
freshKindVar (arg, Nothing) kind = return (ProperName arg, kind)
freshKindVar (arg, Just kind') kind = do
  kind =?= kind'
  return (ProperName arg, kind')

-- |
-- Simultaneously infer the kinds of several mutually recursive type constructors
--
kindsOfAll :: ModuleName -> [(ProperName, [(String, Maybe Kind)], Type)] -> [(ProperName, [(String, Maybe Kind)], [Type])] -> Check ([Kind], [Kind])
kindsOfAll moduleName syns tys = fmap tidyUp . liftUnify $ do
  synVars <- replicateM (length syns) fresh
  let dict = zipWith (\(name, _, _) var -> (name, var)) syns synVars
  bindLocalTypeVariables moduleName dict $ do
    tyCons <- replicateM (length tys) fresh
    let dict' = zipWith (\(name, _, _) tyCon -> (name, tyCon)) tys tyCons
    bindLocalTypeVariables moduleName dict' $ do
      data_ks <- zipWithM (\tyCon (_, args, ts) -> do
        kargs <- replicateM (length args) fresh
        argDict <- zipWithM freshKindVar args kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes True ts kargs tyCon) tyCons tys
      syn_ks <- zipWithM (\synVar (_, args, ty) -> do
        kargs <- replicateM (length args) fresh
        argDict <- zipWithM freshKindVar args kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes False [ty] kargs synVar) synVars syns
      return (syn_ks, data_ks)
  where
  tidyUp ((ks1, ks2), sub) = (map (starIfUnknown . (sub $?)) ks1, map (starIfUnknown . (sub $?)) ks2)

-- |
-- Solve the set of kind constraints associated with the data constructors for a type constructor
--
solveTypes :: Bool -> [Type] -> [Kind] -> Kind -> UnifyT Kind Check Kind
solveTypes isData ts kargs tyCon = do
  ks <- mapM (fmap fst . infer) ts
  when isData $ do
    tyCon =?= foldr FunKind Star kargs
    forM_ ks $ \k -> k =?= Star
  unless isData $
    tyCon =?= foldr FunKind (head ks) kargs
  return tyCon

-- |
-- Default all unknown kinds to the Star kind of types
--
starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (Row k) = Row (starIfUnknown k)
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

-- |
-- Infer a kind for a type
--
infer :: Type -> UnifyT Kind Check (Kind, [(String, Kind)])
infer ty = rethrow (addHint (ErrorCheckingKind ty)) $ infer' ty

infer' :: Type -> UnifyT Kind Check (Kind, [(String, Kind)])
infer' (ForAll ident ty _) = do
  k1 <- fresh
  Just moduleName <- checkCurrentModule <$> get
  (k2, args) <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ infer ty
  k2 =?= Star
  return (Star, (ident, k1) : args)
infer' (KindedType ty k) = do
  (k', args) <- infer ty
  k =?= k'
  return (k', args)
infer' other = (, []) <$> go other
  where
  go :: Type -> UnifyT Kind Check Kind
  go (ForAll ident ty _) = do
    k1 <- fresh
    Just moduleName <- checkCurrentModule <$> get
    k2 <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ go ty
    k2 =?= Star
    return Star
  go (KindedType ty k) = do
    k' <- go ty
    k =?= k'
    return k'
  go TypeWildcard = fresh
  go (TypeVar v) = do
    Just moduleName <- checkCurrentModule <$> get
    UnifyT . lift $ lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (Skolem v _ _) = do
    Just moduleName <- checkCurrentModule <$> get
    UnifyT . lift $ lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
  go (TypeConstructor v) = do
    env <- liftCheck getEnv
    case M.lookup v (types env) of
      Nothing -> UnifyT . lift . throwError . errorMessage $ UnknownTypeConstructor v
      Just (kind, _) -> return kind
  go (TypeApp t1 t2) = do
    k0 <- fresh
    k1 <- go t1
    k2 <- go t2
    k1 =?= FunKind k2 k0
    return k0
  go REmpty = do
    k <- fresh
    return $ Row k
  go (RCons _ ty row) = do
    k1 <- go ty
    k2 <- go row
    k2 =?= Row k1
    return $ Row k1
  go (ConstrainedType deps ty) = do
    forM_ deps $ \(className, tys) -> do
      k <- go $ foldl TypeApp (TypeConstructor className) tys
      k =?= Star
    k <- go ty
    k =?= Star
    return Star
  go _ = internalError "Invalid argument to infer"
