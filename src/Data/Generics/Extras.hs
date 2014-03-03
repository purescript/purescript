-----------------------------------------------------------------------------
--
-- Module      :  Data.Generics.Extras
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Additional SYB combinators
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Data.Generics.Extras where

import Data.Data
import Data.Maybe (fromMaybe)

-- |
-- Apply a top-down monadic transformation everywhere
--
everywhereM' :: (Monad m, Data d) => (forall d1. (Data d1) => d1 -> m d1) -> d -> m d
everywhereM' f x = do
  y <- f x
  gmapM (everywhereM' f) y

-- |
-- Apply a top-down transformation, mutating a state when descending from parents to children
--
-- For example, if we want to relabel bound variables with a different data constructor, we can do so:
--
-- > data Expr = Var String
-- >           | Lam String Test
-- >           | App Test Test
-- >           | LocalVar String deriving (Show, Data, Typeable)
-- >
-- > test = App (Lam "a" (App (Var "a") (Var "b"))) (Var "a")
-- >
-- > varsToLocals :: Expr -> Expr
-- > varsToLocals = everywhereWithContext' [] (mkS go)
-- >   where
-- >   go locals (Var v) | v `elem` locals = (locals, LocalVar v)
-- >   go locals lam@(Lam local _) = (local : locals, lam)
-- >   go locals other = (locals, other)
--
everywhereWithContextM' :: (Monad m, Data d) => s -> (forall d1. (Data d1) => s -> d1 -> m (s, d1)) -> d -> m d
everywhereWithContextM' s0 f x = do
  (s, y) <- f s0 x
  gmapM (everywhereWithContextM' s f) y

-- |
-- Make a stateful transformation function
--
mkS :: (Monad m, Data a, Data b) => (s -> a -> m (s, a)) -> s -> b -> m (s, b)
mkS = extS (curry return)

-- |
-- Extend a stateful transformation function
--
extS :: (Monad m, Data a, Data b) => (s -> a -> m (s, a)) -> (s -> b -> m (s, b)) -> (s -> a -> m (s, a))
extS f g s a = do
  (s', a') <- f s a
  case cast a' of
    Just b -> do
      (s'', b') <- g s' b
      case cast b' of
        Just a'' -> return (s'', a'')
        Nothing -> return (s', a')
    Nothing -> return (s', a')

