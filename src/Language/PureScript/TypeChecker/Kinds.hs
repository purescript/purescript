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
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
import Language.PureScript.Unknown

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import Control.Applicative

import qualified Data.Map as M

instance Unifiable Kind where
  unknown = KUnknown
  isUnknown (KUnknown u) = Just u
  isUnknown _ = Nothing
  KUnknown u1 ~~ KUnknown u2 | u1 == u2 = return ()
  KUnknown u ~~ k = replace u k
  k ~~ KUnknown u = replace u k
  Star ~~ Star = return ()
  Row ~~ Row = return ()
  FunKind k1 k2 ~~ FunKind k3 k4 = do
    k1 ~~ k3
    k2 ~~ k4
  k1 ~~ k2 = throwError $ "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2 ++ "."
  apply s (KUnknown u) = runSubstitution s u
  apply s (FunKind k1 k2) = FunKind (apply s k1) (apply s k2)
  apply _ k = k
  unknowns (KUnknown (Unknown u)) = [u]
  unknowns (FunKind k1 k2) = unknowns k1 ++ unknowns k2
  unknowns _ = []

kindOf :: ModuleName -> Type -> Check Kind
kindOf moduleName ty = fmap (\(k, _, _) -> k) . runSubst (SubstContext moduleName) $ starIfUnknown <$> infer ty

kindsOf :: ModuleName -> ProperName -> [String] -> [PolyType] -> Check Kind
kindsOf moduleName name args ts = fmap (starIfUnknown . (\(k, _, _) -> k)) . runSubst (SubstContext moduleName) $ do
  tyCon <- fresh
  kargs <- replicateM (length args) fresh
  let dict = (name, tyCon) : zip (map ProperName args) kargs
  bindLocalTypeVariables moduleName dict $
    solveTypes ts kargs tyCon

kindsOfAll :: ModuleName -> [(ProperName, [String], [PolyType])] -> Check [Kind]
kindsOfAll moduleName tys = fmap (map starIfUnknown . (\(ks, _, _) -> ks)) . runSubst (SubstContext moduleName) $ do
  tyCons <- replicateM (length tys) fresh
  let dict = zipWith (\(name, _, _) tyCon -> (name, tyCon)) tys tyCons
  bindLocalTypeVariables moduleName dict $
    zipWithM (\tyCon (_, args, ts) -> do
      kargs <- replicateM (length args) fresh
      let argDict = zip (map ProperName args) kargs
      bindLocalTypeVariables moduleName argDict $
        solveTypes ts kargs tyCon) tyCons tys

solveTypes :: [Type] -> [Kind] -> Kind -> Subst Kind
solveTypes ts kargs tyCon = do
  ks <- mapM infer ts
  tyCon ~~ foldr FunKind Star kargs
  forM_ ks $ \k -> k ~~ Star
  return tyCon

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

infer :: Type -> Subst Kind
infer (Array t) = do
  k <- infer t
  k ~~ Star
  return Star
infer (Object row) = do
  k <- inferRow row
  k ~~ Row
  return Star
infer (Function args ret) = do
  ks <- mapM infer args
  k <- infer ret
  k ~~ Star
  forM ks (~~ Star)
  return Star
infer (TypeVar v) = do
  moduleName <- substCurrentModule <$> ask
  lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
infer (TypeConstructor v) = do
  env <- liftCheck getEnv
  moduleName <- substCurrentModule `fmap` ask
  case M.lookup (qualify moduleName v) (types env) of
    Nothing -> throwError $ "Unknown type constructor '" ++ show v ++ "'"
    Just (kind, _) -> return kind
infer (TypeApp t1 t2) = do
  k0 <- fresh
  k1 <- infer t1
  k2 <- infer t2
  k1 ~~ FunKind k2 k0
  return k0
infer (ForAll ident ty) = do
  k <- fresh
  moduleName <- substCurrentModule <$> ask
  bindLocalTypeVariables moduleName [(ProperName ident, k)] $ infer ty
infer _ = return Star

inferRow :: Row -> Subst Kind
inferRow (RowVar v) = do
  moduleName <- substCurrentModule <$> ask
  lookupTypeVariable moduleName (Qualified Nothing (ProperName v))
inferRow REmpty = return Row
inferRow (RCons _ ty row) = do
  k1 <- infer ty
  k2 <- inferRow row
  k1 ~~ Star
  k2 ~~ Row
  return Row
inferRow _ = error "Invalid row in inferRow"
