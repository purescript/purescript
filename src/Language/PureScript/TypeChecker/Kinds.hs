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

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.TypeChecker.Kinds (
    kindsOf,
    kindOf
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Function
import Data.Data

import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty
import Language.PureScript.Unknown

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***))
import qualified Control.Category as C

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

kindOf :: Type -> Check Kind
kindOf ty = runSubst $ starIfUnknown <$> infer Nothing M.empty ty

kindsOf :: Maybe ProperName -> [String] -> [Type] -> Check Kind
kindsOf name args ts = fmap starIfUnknown $ runSubst $ do
  tyCon <- fresh
  kargs <- replicateM (length args) fresh
  ks <- inferAll (fmap (\pn -> (pn, tyCon)) name) (M.fromList (zip args kargs)) ts
  tyCon ~~ foldr FunKind Star kargs
  forM_ ks $ \k -> k ~~ Star
  return tyCon

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

inferAll :: Maybe (ProperName, Kind) -> M.Map String Kind -> [Type] -> Subst Check [Kind]
inferAll name m = mapM (infer name m)

infer :: Maybe (ProperName, Kind) -> M.Map String Kind -> Type -> Subst Check Kind
infer name m (Array t) = do
  k <- infer name m t
  k ~~ Star
  return Star
infer name m (Object row) = do
  k <- inferRow name m row
  k ~~ Row
  return Star
infer name m (Function args ret) = do
  ks <- inferAll name m args
  k <- infer name m ret
  k ~~ Star
  forM ks $ \k -> k ~~ Star
  return Star
infer _ m (TypeVar v) =
  case M.lookup v m of
    Just k -> return k
    Nothing -> throwError $ "Unbound type variable " ++ v
infer (Just (name, k)) m c@(TypeConstructor v@(Qualified (ModulePath []) pn)) | name == pn = return k
infer name m (TypeConstructor v) = do
  env <- lift getEnv
  modulePath <- checkModulePath `fmap` lift get
  case M.lookup (qualify modulePath v) (types env) of
    Nothing -> throwError $ "Unknown type constructor '" ++ show v ++ "'"
    Just (kind, _) -> return kind
infer name m (TypeApp t1 t2) = do
  k0 <- fresh
  k1 <- infer name m t1
  k2 <- infer name m t2
  k1 ~~ FunKind k2 k0
  return k0
infer name m (ForAll ident ty) = do
  k <- fresh
  infer name (M.insert ident k m) ty
infer _ m t = return Star

inferRow :: Maybe (ProperName, Kind) -> M.Map String Kind -> Row -> Subst Check Kind
inferRow _ m (RowVar v) = do
  case M.lookup v m of
    Just k -> return k
    Nothing -> throwError $ "Unbound row variable " ++ v
inferRow _ m r@REmpty = return Row
inferRow name m r@(RCons _ ty row) = do
  k1 <- infer name m ty
  k2 <- inferRow name m row
  k1 ~~ Star
  k2 ~~ Row
  return Row
