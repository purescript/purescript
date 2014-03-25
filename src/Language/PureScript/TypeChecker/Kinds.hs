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

module Language.PureScript.TypeChecker.Kinds (
    kindOf,
    kindsOf,
    kindsOfAll
) where

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty
import Language.PureScript.Environment

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Unify

import Control.Applicative

import qualified Data.Map as M

instance Partial Kind where
  unknown = KUnknown
  isUnknown (KUnknown u) = Just u
  isUnknown _ = Nothing

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
  k1 =?= k2 = UnifyT . lift . throwError $ "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2 ++ "."

-- |
-- Infer the kind of a single type
--
kindOf :: ModuleName -> Type -> Check Kind
kindOf _ ty =
  rethrow (("Error checking kind of " ++ prettyPrintType ty ++ ":\n") ++) $
    fmap tidyUp . liftUnify $ starIfUnknown <$> infer ty
  where
  tidyUp (k, sub) = sub $? k

-- |
-- Infer the kind of a type constructor with a collection of arguments and a collection of associated data constructors
--
kindsOf :: Bool -> ModuleName -> ProperName -> [String] -> [Type] -> Check Kind
kindsOf isData moduleName name args ts = fmap tidyUp . liftUnify $ do
  tyCon <- fresh
  kargs <- replicateM (length args) fresh
  let dict = (name, tyCon) : zipWith (\arg kind -> (arg, kind)) (map ProperName args) kargs
  bindLocalTypeVariables moduleName dict $
    solveTypes isData ts kargs tyCon
  where
  tidyUp (k, sub) = starIfUnknown $ sub $? k

-- |
-- Simultaneously infer the kinds of several mutually recursive type constructors
--
kindsOfAll :: ModuleName -> [(ProperName, [String], Type)] -> [(ProperName, [String], [Type])] -> Check ([Kind], [Kind])
kindsOfAll moduleName syns tys = fmap tidyUp . liftUnify $ do
  synVars <- replicateM (length syns) fresh
  let dict = zipWith (\(name, _, _) var -> (name, var)) syns synVars
  bindLocalTypeVariables moduleName dict $ do
    tyCons <- replicateM (length tys) fresh
    let dict' = zipWith (\(name, _, _) tyCon -> (name, tyCon)) tys tyCons
    bindLocalTypeVariables moduleName dict' $ do
      data_ks <- zipWithM (\tyCon (_, args, ts) -> do
        kargs <- replicateM (length args) fresh
        let argDict = zip (map ProperName args) kargs
        bindLocalTypeVariables moduleName argDict $
          solveTypes True ts kargs tyCon) tyCons tys
      syn_ks <- zipWithM (\synVar (_, args, ty) -> do
        kargs <- replicateM (length args) fresh
        let argDict = zip (map ProperName args) kargs
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
  ks <- mapM infer ts
  when isData $ do
    tyCon =?= foldr FunKind Star kargs
    forM_ ks $ \k -> k =?= Star
  when (not isData) $ do
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
infer :: Type -> UnifyT Kind Check Kind
infer (TypeVar v) = do
  Just moduleName <- checkCurrentModule <$> get
  UnifyT . lift $ lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
infer (TypeConstructor v) = do
  env <- liftCheck getEnv
  case M.lookup v (types env) of
    Nothing -> UnifyT . lift . throwError $ "Unknown type constructor '" ++ show v ++ "'"
    Just (kind, _) -> return kind
infer (TypeApp t1 t2) = do
  k0 <- fresh
  k1 <- infer t1
  k2 <- infer t2
  k1 =?= FunKind k2 k0
  return k0
infer (ForAll ident ty _) = do
  k1 <- fresh
  Just moduleName <- checkCurrentModule <$> get
  k2 <- bindLocalTypeVariables moduleName [(ProperName ident, k1)] $ infer ty
  k2 =?= Star
  return Star
infer REmpty = do
  k <- fresh
  return $ Row k
infer (RCons _ ty row) = do
  k1 <- infer ty
  k2 <- infer row
  k2 =?= Row k1
  return $ Row k1
infer (ConstrainedType deps ty) = do
  forM_ deps $ \(className, tys) -> do
    _ <- infer $ foldl TypeApp (TypeConstructor className) tys
    return ()
  k <- infer ty
  k =?= Star
  return Star
infer _ = error "Invalid argument to infer"
