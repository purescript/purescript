-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Unify
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Unify where

import Data.Data
import Data.Maybe
import Data.Monoid
import Data.Generics (mkT, mkQ, everywhere, everything)

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import Data.HashMap.Strict as M

-- |
-- Untyped unification variables
--
newtype Unknown = Unknown {
  -- |
  -- The underlying integer representing the unification variable
  --
    runUnknown :: Int
  } deriving (Show, Eq, Ord, Data, Typeable)

-- |
-- A type which can contain unification variables
--
class (Typeable t, Data t) => Partial t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown

-- |
-- Identifies types which support unification
--
class (Partial t) => Unifiable m t | t -> m where
  (=?=) :: t -> t -> UnifyT t m ()

-- |
-- A substitution maintains a mapping from unification variables to their values
--
data Substitution t = Substitution { runSubstitution :: M.HashMap Int t }

instance (Partial t) => Monoid (Substitution t) where
  mempty = Substitution M.empty
  s1 `mappend` s2 = Substitution $
                      M.map (s2 $?) (runSubstitution s1) `M.union`
                      M.map (s1 $?) (runSubstitution s2)

-- |
-- Apply a substitution to a value
--
($?) :: (Partial t) => Substitution t -> t -> t
($?) sub = everywhere (mkT go)
  where
  go t =
    case isUnknown t of
      Nothing -> t
      Just (Unknown u) -> case M.lookup u (runSubstitution sub) of
                  Nothing -> t
                  Just t' -> t'

-- |
-- State required for type checking
--
data UnifyState t = UnifyState {
  -- |
  -- The next fresh unification variable
  --
    unifyNextVar :: Int
  -- |
  -- The current substitution
  --
  , unifyCurrentSubstitution :: Substitution t
  }

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: (Partial t) => UnifyState t
defaultUnifyState = UnifyState 0 mempty

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype UnifyT t m a = UnifyT { unUnify :: StateT (UnifyState t) (ErrorT String m) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadError String)

instance (MonadState s m) => MonadState s (UnifyT t m) where
  get = UnifyT . lift $ get
  put = UnifyT . lift . put

-- |
-- Collect all unknowns occurring inside a value
--
unknowns :: (Data d) => d -> [Unknown]
unknowns = everything (++) (mkQ [] collect)
  where
  collect u@(Unknown _) = [u]

-- |
-- Run a computation in the Unify monad, failing with an error, or succeeding with a return value and the new next unification variable
--
runUnify :: UnifyState t -> UnifyT t m a -> m (Either String (a, UnifyState t))
runUnify s = runErrorT . flip runStateT s . unUnify

-- |
-- Substitute a single unification variable
--
substituteOne :: (Partial t) => Unknown -> t -> Substitution t
substituteOne (Unknown u) t = Substitution $ M.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
--
(=:=) :: (Monad m, Unifiable m t) => Unknown -> t -> UnifyT t m ()
(=:=) u t' = do
  st <- UnifyT get
  let sub = unifyCurrentSubstitution st
  let t = sub $? t'
  occursCheck u t
  let current = sub $? unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return ()
    _ -> current =?= t
  UnifyT $ modify $ \s -> s { unifyCurrentSubstitution = substituteOne u t <> unifyCurrentSubstitution s }

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: (Monad m, Partial t) => Unknown -> t -> UnifyT t m ()
occursCheck u t =
  case isUnknown t of
    Nothing -> when (u `elem` unknowns t) $ UnifyT . lift $ throwError "Occurs check fails"
    _ -> return ()

-- |
-- Generate a fresh untyped unification variable
--
fresh' :: (Monad m) => UnifyT t m Unknown
fresh' = do
  st <- UnifyT get
  UnifyT $ modify $ \s -> s { unifyNextVar = succ (unifyNextVar s) }
  return $ Unknown (unifyNextVar st)

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: (Monad m, Partial t) => UnifyT t m t
fresh = do
  u <- fresh'
  return $ unknown u

