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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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
-- The type of typed unification variables
--
newtype TypedUnknown t = TypedUnknown {
  -- |
  -- The underlying untyped unification variable
  --
    runTypedUnknown :: Unknown
  } deriving (Show, Eq, Ord, Data, Typeable)

-- |
-- Identifies types which support unification
--
class (Typeable t, Data t, Show t) => Unifiable m t | t -> m where
  unknown :: TypedUnknown t -> t
  isUnknown :: t -> Maybe (TypedUnknown t)
  (?=) :: t -> t -> UnifyT m ()

-- |
-- A substitution maintains a mapping from unification variables to their values, ensuring that
-- the type of a unification variable matches the type of its value.
--
newtype Substitution = Substitution { runSubstitution :: forall d. (Data d) => d -> d }

instance Monoid Substitution where
  mempty = Substitution id
  s1 `mappend` s2 = Substitution $ runSubstitution s1 . runSubstitution s2

-- |
-- State required for type checking:
--
data UnifyState = UnifyState {
  -- |
  -- The next fresh unification variable
  --
    unifyNextVar :: Int
  -- |
  -- The current substitution
  --
  , unifyCurrentSubstitution :: Substitution
  }

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: UnifyState
defaultUnifyState = UnifyState 0 mempty

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
newtype UnifyT m a = UnifyT { unUnify :: StateT UnifyState (ErrorT String m) a }
  deriving (Functor, Monad, Applicative, MonadPlus, MonadError String)

instance (MonadState s m) => MonadState s (UnifyT m) where
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
runUnify :: UnifyState -> UnifyT m a -> m (Either String (a, UnifyState))
runUnify s = runErrorT . flip runStateT s . unUnify

-- |
-- Substitute a single unification variable
--
substituteOne :: (Unifiable m t) => TypedUnknown t -> t -> Substitution
substituteOne u t = Substitution $ everywhere (mkT go)
  where
  go t' = case isUnknown t' of
           Just u1 | u1 == u -> t
           _ -> t'

-- |
-- Replace a unification variable with the specified value in the current substitution
--
replace :: (Monad m, Unifiable m t) => TypedUnknown t -> t -> UnifyT m ()
replace u t' = do
  st <- UnifyT get
  let sub = unifyCurrentSubstitution st
  let t = runSubstitution sub t'
  occursCheck u t
  let current = runSubstitution sub $ unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return ()
    _ -> current ?= t
  UnifyT $ modify $ \s -> s { unifyCurrentSubstitution = substituteOne u t <> unifyCurrentSubstitution s }

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: (Monad m, Unifiable m t) => TypedUnknown s -> t -> UnifyT m ()
occursCheck (TypedUnknown u) t =
  case isUnknown t of
    Nothing -> when (u `elem` unknowns t) $ UnifyT . lift $ throwError "Occurs check fails"
    _ -> return ()

-- |
-- Generate a fresh untyped unification variable
--
fresh' :: (Monad m) => UnifyT m Unknown
fresh' = do
  st <- UnifyT get
  UnifyT $ modify $ \s -> s { unifyNextVar = succ (unifyNextVar s) }
  return $ Unknown (unifyNextVar st)

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: (Monad m, Unifiable m t) => UnifyT m t
fresh = do
  u <- fresh'
  return $ unknown $ TypedUnknown u

