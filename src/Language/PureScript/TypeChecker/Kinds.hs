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
    KindConstraint(..),
    KindSolution(..),
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

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***))
import qualified Control.Category as C

import qualified Data.Map as M

data KindConstraintOrigin
  = DataDeclOrigin
  | TypeOrigin Type
  | RowOrigin Row deriving (Show, Data, Typeable)

prettyPrintKindConstraintOrigin :: KindConstraintOrigin -> String
prettyPrintKindConstraintOrigin (DataDeclOrigin) = "data declaration"
prettyPrintKindConstraintOrigin (TypeOrigin ty) = prettyPrintType ty
prettyPrintKindConstraintOrigin (RowOrigin row) = prettyPrintRow row

data KindConstraint = KindConstraint Int Kind KindConstraintOrigin deriving (Show, Data, Typeable)

newtype KindSolution = KindSolution { runKindSolution :: Int -> Kind }

emptyKindSolution :: KindSolution
emptyKindSolution = KindSolution KUnknown

kindOf :: Type -> Check Kind
kindOf ty = do
  (cs, n, m) <- kindConstraints Nothing M.empty ty
  solution <- solveKindConstraints cs emptyKindSolution
  return $ starIfUnknown $ runKindSolution solution n

kindsOf :: Maybe ProperName -> [String] -> [Type] -> Check Kind
kindsOf name args ts = do
  tyCon <- fresh
  nargs <- replicateM (length args) fresh
  (cs, ns, m) <- kindConstraintsAll (fmap (\pn -> (pn, tyCon)) name) (M.fromList (zip args nargs)) ts
  let extraConstraints =
        KindConstraint tyCon (foldr (FunKind . KUnknown) Star nargs) DataDeclOrigin
        : zipWith (\n arg -> KindConstraint n Star (TypeOrigin arg)) ns ts
  solution <- solveKindConstraints (extraConstraints ++ cs) emptyKindSolution
  return $ starIfUnknown $ runKindSolution solution tyCon

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

kindConstraintsAll :: Maybe (ProperName, Int) -> M.Map String Int -> [Type] -> Check ([KindConstraint], [Int], M.Map String Int)
kindConstraintsAll _ m [] = return ([], [], m)
kindConstraintsAll name m (t:ts) = do
  (cs, n1, m') <- kindConstraints name m t
  (cs', ns, m'') <- kindConstraintsAll name m' ts
  return (KindConstraint n1 Star (TypeOrigin t) : cs ++ cs', n1:ns, m'')

kindConstraints :: Maybe (ProperName, Int) -> M.Map String Int -> Type -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraints name m a@(Array t) = do
  me <- fresh
  (cs, n1, m') <- kindConstraints name m t
  return (KindConstraint n1 Star (TypeOrigin t) : KindConstraint me Star (TypeOrigin a) : cs, me, m')
kindConstraints name m o@(Object row) = do
  me <- fresh
  (cs, r, m') <- kindConstraintsForRow name m row
  return (KindConstraint me Star (TypeOrigin o) : KindConstraint r Row (RowOrigin row) : cs, me, m')
kindConstraints name m f@(Function args ret) = do
  me <- fresh
  (cs, ns, m') <- kindConstraintsAll name m args
  (cs', retN, m'') <- kindConstraints name m' ret
  return (KindConstraint retN Star (TypeOrigin ret) : KindConstraint me Star (TypeOrigin f) : zipWith (\n arg -> KindConstraint n Star (TypeOrigin arg)) ns args ++ cs ++ cs', me, m'')
kindConstraints _ m (TypeVar v) =
  case M.lookup v m of
    Just u -> return ([], u, m)
    Nothing -> throwError $ "Unbound type variable " ++ v
kindConstraints (Just (name, u)) m c@(TypeConstructor v@(Qualified (ModulePath []) pn)) | name == pn = do
  env <- getEnv
  me <- fresh
  modulePath <- checkModulePath `fmap` get
  return ([KindConstraint me (KUnknown u) (TypeOrigin c)], me, m)
kindConstraints name m c@(TypeConstructor v) = do
  env <- getEnv
  me <- fresh
  modulePath <- checkModulePath `fmap` get
  case M.lookup (qualify modulePath v) (types env) of
    Nothing -> throwError $ "Unknown type constructor '" ++ show v ++ "'"
    Just (kind, _) -> return ([KindConstraint me kind (TypeOrigin c)], me, m)
kindConstraints name m a@(TypeApp t1 t2) = do
  me <- fresh
  (cs1, n1, m1) <- kindConstraints name m t1
  (cs2, n2, m2) <- kindConstraints name m1 t2
  return (KindConstraint n1 (FunKind (KUnknown n2) (KUnknown me)) (TypeOrigin a) : cs1 ++ cs2, me, m2)
kindConstraints name m (ForAll idents ty) = do
  ns <- replicateM (length idents) fresh
  kindConstraints name (M.fromList (zip idents ns) `M.union` m) ty
kindConstraints _ m t = do
  me <- fresh
  return ([KindConstraint me Star (TypeOrigin t)], me, m)

kindConstraintsForRow :: Maybe (ProperName, Int) -> M.Map String Int -> Row -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraintsForRow _ m r@(RowVar v) = do
  me <- case M.lookup v m of
    Just u -> return u
    Nothing -> fresh
  return ([KindConstraint me Row (RowOrigin r)], me, M.insert v me m)
kindConstraintsForRow _ m r@REmpty = do
  me <- fresh
  return ([KindConstraint me Row (RowOrigin r)], me, m)
kindConstraintsForRow name m r@(RCons _ ty row) = do
  me <- fresh
  (cs1, n1, m1) <- kindConstraints name m ty
  (cs2, n2, m2) <- kindConstraintsForRow name m1 row
  return (KindConstraint me Row (RowOrigin r) : KindConstraint n1 Star (TypeOrigin ty) : KindConstraint n2 Row (RowOrigin r) : cs1 ++ cs2, me, m2)

solveKindConstraints :: [KindConstraint] -> KindSolution -> Check KindSolution
solveKindConstraints [] s = return s
solveKindConstraints all@(KindConstraint n k t : cs) s = do
  (cs', s') <- rethrow (\err -> "Error in " ++ prettyPrintKindConstraintOrigin t ++ ": " ++ err) $ do
    guardWith "Occurs check failed" $ not $ kindOccursCheck False n k
    let s' = KindSolution $ replaceUnknownKind n k . runKindSolution s
    cs' <- fmap concat $ mapM (substituteKindConstraint n k) cs
    return (cs', s')
  solveKindConstraints cs' s'

substituteKindConstraint :: Int -> Kind -> KindConstraint -> Check [KindConstraint]
substituteKindConstraint n k (KindConstraint m l t)
  | n == m = unifyKinds t k l
  | otherwise = return [KindConstraint m (replaceUnknownKind n k l) t]

replaceUnknownKind :: Int -> Kind -> Kind -> Kind
replaceUnknownKind n k = f
  where
  f (KUnknown m) | m == n = k
  f (FunKind k1 k2) = FunKind (f k2) (f k2)
  f other = other

unifyKinds :: KindConstraintOrigin -> Kind -> Kind -> Check [KindConstraint]
unifyKinds _ (KUnknown u1) (KUnknown u2) | u1 == u2 = return []
unifyKinds t (KUnknown u) k = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck False u k
  return [KindConstraint u k t]
unifyKinds t k (KUnknown u) = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck False u k
  return [KindConstraint u k t]
unifyKinds _ Star Star = return []
unifyKinds _ Row Row = return []
unifyKinds t (FunKind k1 k2) (FunKind k3 k4) = do
  cs1 <- unifyKinds t k1 k3
  cs2 <- unifyKinds t k2 k4
  return $ cs1 ++ cs2
unifyKinds _ k1 k2 = throwError $ "Cannot unify " ++ prettyPrintKind k1 ++ " with " ++ prettyPrintKind k2 ++ "."

kindOccursCheck :: Bool -> Int -> Kind -> Bool
kindOccursCheck b u (KUnknown u') | u == u' = b
kindOccursCheck _ u (FunKind k1 k2) = kindOccursCheck True u k1 || kindOccursCheck True u k2
kindOccursCheck _ _ _ = False
