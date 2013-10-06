-----------------------------------------------------------------------------
--
-- Module      :  PureScript.TypeChecker.Kinds
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

module PureScript.TypeChecker.Kinds (
    KindConstraint(..),
    KindSolution(..),
    kindsOf,
    kindOf
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Function

import PureScript.Types
import PureScript.Kinds
import PureScript.Declarations
import PureScript.TypeChecker.Monad

import Control.Monad.State
import Control.Monad.Error

import Control.Applicative
import Control.Arrow (Kleisli(..), (***))
import qualified Control.Category as C

import qualified Data.Map as M

data KindConstraint = KindConstraint Int Kind

instance Show KindConstraint where
  show (KindConstraint n k) = show n ++ " = " ++ show k

newtype KindSolution = KindSolution { runKindSolution :: Int -> Kind }

emptyKindSolution :: KindSolution
emptyKindSolution = KindSolution KUnknown

kindOf :: Type -> Check Kind
kindOf ty = do
  (cs, n, m) <- kindConstraints M.empty ty
  solution <- solveKindConstraints cs emptyKindSolution
  return $ starIfUnknown $ runKindSolution solution n

kindsOf :: Maybe String -> [String] -> [Type] -> Check Kind
kindsOf name args ts = do
  tyCon <- fresh
  nargs <- replicateM (length args) fresh
  (cs, ns, m) <- kindConstraintsAll (maybe id (flip M.insert tyCon) name $ M.fromList (zipWith (,) args nargs)) ts
  let extraConstraints = KindConstraint tyCon (foldr FunKind Star (map KUnknown nargs)) : map (flip KindConstraint Star) ns
  solution <- solveKindConstraints (extraConstraints ++ cs) emptyKindSolution
  return $ starIfUnknown $ runKindSolution solution tyCon

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

kindConstraintsAll :: M.Map String Int -> [Type] -> Check ([KindConstraint], [Int], M.Map String Int)
kindConstraintsAll m [] = return ([], [], m)
kindConstraintsAll m (t:ts) = do
  (cs, n1, m') <- kindConstraints m t
  (cs', ns, m'') <- kindConstraintsAll m' ts
  return ((KindConstraint n1 Star) : cs ++ cs', n1:ns, m'')

kindConstraints :: M.Map String Int -> Type -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraints m (Array t) = do
  me <- fresh
  (cs, n1, m') <- kindConstraints m t
  return ((KindConstraint n1 Star) : (KindConstraint me Star) : cs, me, m')
kindConstraints m (Object row) = do
  me <- fresh
  (cs, row, m') <- kindConstraintsForRow m row
  return ((KindConstraint me Star) : (KindConstraint row Row) : cs, me, m')
kindConstraints m (Function args ret) = do
  me <- fresh
  (cs, ns, m') <- kindConstraintsAll m args
  (cs', retN, m'') <- kindConstraints m' ret
  return ((KindConstraint retN Star) : (KindConstraint me Star) : (map (flip KindConstraint Star) ns) ++ cs, me, m'')
kindConstraints m (TypeVar v) = do
  case M.lookup v m of
    Just u -> return ([], u, m)
    Nothing -> throwError $ "Unbound type variable " ++ v
kindConstraints m (TypeConstructor v) = do
  env <- getEnv
  me <- fresh
  case M.lookup v m of
    Nothing -> case M.lookup v (types env) of
      Nothing -> throwError $ "Unknown type constructor '" ++ v ++ "'"
      Just (kind, _) -> return ([KindConstraint me kind], me, m)
    Just u -> do
      return ([KindConstraint me (KUnknown u)], me, m)
kindConstraints m (TypeApp t1 t2) = do
  me <- fresh
  (cs1, n1, m1) <- kindConstraints m t1
  (cs2, n2, m2) <- kindConstraints m1 t2
  return ((KindConstraint n1 (FunKind (KUnknown n2) (KUnknown me))) : cs1 ++ cs2, me, m2)
kindConstraints m (ForAll idents ty) = do
  ns <- replicateM (length idents) fresh
  kindConstraints (m `M.union` M.fromList (zip idents ns)) ty
kindConstraints m _ = do
  me <- fresh
  return ([KindConstraint me Star], me, m)

kindConstraintsForRow :: M.Map String Int -> Row -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraintsForRow m (RowVar v) = do
  me <- case M.lookup v m of
    Just u -> return u
    Nothing -> fresh
  return ([KindConstraint me Row], me, M.insert v me m)
kindConstraintsForRow m REmpty = do
  me <- fresh
  return ([KindConstraint me Row], me, m)
kindConstraintsForRow m (RCons _ ty row) = do
  me <- fresh
  (cs1, n1, m1) <- kindConstraints m ty
  (cs2, n2, m2) <- kindConstraintsForRow m1 row
  return ((KindConstraint me Row) : (KindConstraint n1 Star) : (KindConstraint n2 Row) : cs1 ++ cs2, me, m2)

solveKindConstraints :: [KindConstraint] -> KindSolution -> Check KindSolution
solveKindConstraints [] s = return s
solveKindConstraints all@(KindConstraint n k:cs) s = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck False n k
  let s' = KindSolution $ replaceUnknownKind n k . runKindSolution s
  cs' <- fmap concat $ mapM (substituteKindConstraint n k) cs
  solveKindConstraints cs' s'

substituteKindConstraint :: Int -> Kind -> KindConstraint -> Check [KindConstraint]
substituteKindConstraint n k (KindConstraint m l)
  | n == m = unifyKinds k l
  | otherwise = return [KindConstraint m (replaceUnknownKind n k l)]

replaceUnknownKind :: Int -> Kind -> Kind -> Kind
replaceUnknownKind n k = f
  where
  f (KUnknown m) | m == n = k
  f (FunKind k1 k2) = FunKind (f k2) (f k2)
  f other = other

unifyKinds :: Kind -> Kind -> Check [KindConstraint]
unifyKinds (KUnknown u1) (KUnknown u2) | u1 == u2 = return []
unifyKinds (KUnknown u) k = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck False u k
  return [KindConstraint u k]
unifyKinds k (KUnknown u) = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck False u k
  return [KindConstraint u k]
unifyKinds Star Star = return []
unifyKinds Row Row = return []
unifyKinds (FunKind k1 k2) (FunKind k3 k4) = do
  cs1 <- unifyKinds k1 k3
  cs2 <- unifyKinds k2 k4
  return $ cs1 ++ cs2
unifyKinds k1 k2 = throwError $ "Cannot unify " ++ show k1 ++ " with " ++ show k2 ++ "."

kindOccursCheck :: Bool -> Int -> Kind -> Bool
kindOccursCheck b u (KUnknown u') | u == u' = b
kindOccursCheck _ u (FunKind k1 k2) = kindOccursCheck True u k1 || kindOccursCheck True u k2
kindOccursCheck _ _ _ = False
