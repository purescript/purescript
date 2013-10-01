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
    kindsOf
) where

import Data.List
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

kindsOf :: String -> [String] -> [Type] -> Check Kind
kindsOf name args ts = do
  let tyCon = 0
  let nargs = [1..length args]
  (cs, ns, _, m) <- kindConstraintsAll (length args + 1) (M.insert name tyCon $ M.fromList (zipWith (,) args nargs)) ts
  let extraConstraints = KindConstraint tyCon (foldr FunKind Star (map KUnknown nargs)) : map (flip KindConstraint Star) ns
  solution <- solveKindConstraints (extraConstraints ++ cs) emptyKindSolution
  return $ starIfUnknown $ runKindSolution solution 0

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

kindConstraintsAll :: Int -> M.Map String Int -> [Type] -> Check ([KindConstraint], [Int], Int, M.Map String Int)
kindConstraintsAll n m [] = return ([], [], n, m)
kindConstraintsAll n m (t:ts) = do
  (cs, n1, max1, m') <- kindConstraints n m t
  (cs', ns, max2, m'') <- kindConstraintsAll (max1 + 1) m' ts
  return ((KindConstraint n1 Star) : cs ++ cs', n1:ns, max2, m'')

kindConstraints :: Int -> M.Map String Int -> Type -> Check ([KindConstraint], Int, Int, M.Map String Int)
kindConstraints n m (Array t) = do
  let me = n
  (cs, n1, max1, m') <- kindConstraints (n + 1) m t
  return ((KindConstraint n1 Star) : (KindConstraint me Star) : cs, me, max1, m')
kindConstraints n m (Object row) = do
  let me = n
  (cs, row, max1, m') <- kindConstraintsForRow (n + 1) m row
  return ((KindConstraint me Star) : (KindConstraint row Row) : cs, me, max1, m')
kindConstraints n m (Function args ret) = do
  let me = n
  (cs, ns, max1, m') <- kindConstraintsAll (n + 1) m args
  (cs', retN, max2, m'') <- kindConstraints (max1 + 1) m' ret
  return ((KindConstraint retN Star) : (KindConstraint me Star) : (map (flip KindConstraint Star) ns) ++ cs, me, max2, m'')
kindConstraints n m (TypeVar v) =
  let n' = maybe n id (M.lookup v m)
  in return ([], n', n, M.insert v n' m)
kindConstraints n m (TypeConstructor v) = do
  env <- get
  kind <- case M.lookup v m of
    Nothing -> case M.lookup v (types env) of
      Nothing -> throwError $ "Unknown type constructor '" ++ v ++ "'"
      Just k -> return k
    Just u -> return (KUnknown u)
  return ([KindConstraint n kind], n, n, m)
kindConstraints n m (TypeApp t1 t2) = do
  let me = n
  (cs1, n1, max1, m1) <- kindConstraints (n + 1) m t1
  (cs2, n2, max2, m2) <- kindConstraints (max1 + 1) m1 t2
  return ((KindConstraint n1 (FunKind (KUnknown n2) (KUnknown me))) : cs1 ++ cs2, me, max2, m2)
kindConstraints n m _ = return ([KindConstraint n Star], n, n, m)

kindConstraintsForRow :: Int -> M.Map String Int -> Row -> Check ([KindConstraint], Int, Int, M.Map String Int)
kindConstraintsForRow n m (RowVar v) =
  let n' = maybe n id (M.lookup v m)
  in return ([KindConstraint n' Row], n', n, M.insert v n' m)
kindConstraintsForRow n m REmpty = return ([KindConstraint n Row], n, n, m)
kindConstraintsForRow n m (RCons _ ty row) = do
  (cs1, n1, max1, m1) <- kindConstraints (n + 1) m ty
  (cs2, n2, max2, m2) <- kindConstraintsForRow (max1 + 1) m1 row
  return ((KindConstraint n Row) : (KindConstraint n1 Star) : (KindConstraint n2 Row) : cs1 ++ cs2, n, max2, m2)

solveKindConstraints :: [KindConstraint] -> KindSolution -> Check KindSolution
solveKindConstraints [] s = return s
solveKindConstraints all@(KindConstraint n k:cs) s = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck n k
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
  guardWith "Occurs check failed" $ not $ kindOccursCheck u k
  return [KindConstraint u k]
unifyKinds k (KUnknown u) = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck u k
  return [KindConstraint u k]
unifyKinds Star Star = return []
unifyKinds Row Row = return []
unifyKinds (FunKind k1 k2) (FunKind k3 k4) = do
  cs1 <- unifyKinds k1 k3
  cs2 <- unifyKinds k2 k4
  return $ cs1 ++ cs2
unifyKinds k1 k2 = throwError $ "Cannot unify " ++ show k1 ++ " with " ++ show k2 ++ "."

kindOccursCheck :: Int -> Kind -> Bool
kindOccursCheck u (KUnknown u') | u == u' = True
kindOccursCheck u (FunKind k1 k2) = kindOccursCheck u k1 || kindOccursCheck u k2
kindOccursCheck _ _ = False
