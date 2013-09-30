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

data KindConstraint = KindConstraint Int Kind deriving Show

newtype KindSolution = KindSolution { runKindSolution :: Int -> Kind }

emptyKindSolution :: KindSolution
emptyKindSolution = KindSolution KUnknown

kindsOf :: [String] -> [Type] -> Check [Kind]
kindsOf args ts = do
  (cs, _, m) <- kindConstraintsAll 0 M.empty ts
  solution <- solveKindConstraints cs emptyKindSolution
  return $ map (maybe Star starIfUnknown . fmap (runKindSolution solution) . flip M.lookup m) args

starIfUnknown :: Kind -> Kind
starIfUnknown (KUnknown _) = Star
starIfUnknown (FunKind k1 k2) = FunKind (starIfUnknown k1) (starIfUnknown k2)
starIfUnknown k = k

kindConstraintsAll :: Int -> M.Map String Int -> [Type] -> Check ([KindConstraint], [Int], M.Map String Int)
kindConstraintsAll _ m [] = return ([], [], m)
kindConstraintsAll n m (t:ts) = do
  (cs, n', m') <- kindConstraints n m t
  (cs', ns, m'') <- kindConstraintsAll (n' + 1) m' ts
  return ((KindConstraint n' Star) : cs ++ cs', n':ns, m'')

kindConstraints :: Int -> M.Map String Int -> Type -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraints n m (Array t) = do
  (cs, n', m') <- kindConstraints n m t
  return ((KindConstraint n' Star) : (KindConstraint (n' + 1) Star) : cs, n' + 1, m')
kindConstraints n m (Object row) = do
  let me = n
  (cs, row, m') <- kindConstraintsForRow (n + 2) m row
  return ((KindConstraint me Star) : (KindConstraint row Row) : cs, me, m')
kindConstraints n m (Function args ret) = do
  (cs, ns, m') <- kindConstraintsAll n m args
  let next = maximum ns + 1
  (cs', retN, m'') <- kindConstraints next m' ret
  let me = retN + 1
  return ((KindConstraint retN Star) : (KindConstraint me Star) : (map (flip KindConstraint Star) ns) ++ cs, me, m'')
kindConstraints n m (TypeVar v) =
  let
    n' = maybe n id (M.lookup v m)
  in return ([], n', M.insert v n' m)
kindConstraints n m (TypeConstructor v) = do
  env <- get
  kind <- maybe (throwError "Unknown type constructor") return (M.lookup v (types env))
  return ([KindConstraint n kind], n, m)
kindConstraints n m (TypeApp t1 t2) = do
  (cs1, n1, m1) <- kindConstraints n m t1
  (cs2, n2, m2) <- kindConstraints (n1 + 1) m1 t2
  return ((KindConstraint n1 (FunKind (KUnknown n2) (KUnknown (n2 + 1)))) : cs1 ++ cs2, n2 + 1, m2)
kindConstraints n m _ = return ([KindConstraint n Star], n, m)

kindConstraintsForRow :: Int -> M.Map String Int -> Row -> Check ([KindConstraint], Int, M.Map String Int)
kindConstraintsForRow = undefined

solveKindConstraints :: [KindConstraint] -> KindSolution -> Check KindSolution
solveKindConstraints [] s = return s
solveKindConstraints (KindConstraint n k:cs) s = do
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
unifyKinds (KUnknown u) k = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck u k
  return [KindConstraint u k]
unifyKinds k (KUnknown u) = do
  guardWith "Occurs check failed" $ not $ kindOccursCheck u k
  return [KindConstraint u k]
unifyKinds Star Star = return []
unifyKinds (FunKind k1 k2) (FunKind k3 k4) = do
  cs1 <- unifyKinds k1 k3
  cs2 <- unifyKinds k2 k4
  return $ cs1 ++ cs2
unifyKinds k1 k2 = throwError $ "Cannot unify " ++ show k1 ++ " with " ++ show k2 ++ "."

kindOccursCheck :: Int -> Kind -> Bool
kindOccursCheck u (KUnknown u') | u == u' = True
kindOccursCheck u (FunKind k1 k2) = kindOccursCheck u k1 || kindOccursCheck u k2
kindOccursCheck _ _ = False
