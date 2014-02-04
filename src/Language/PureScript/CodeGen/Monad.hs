-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Monad
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Code generation monad
--
-- This monad provides a supply of fresh names which can be used to create variables.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.CodeGen.Monad where

import Control.Monad.State
import Control.Applicative
import Language.PureScript.Names

-- |
-- Code generation monad data type
--
newtype Gen a = Gen { unGen :: State [String] a } deriving (Functor, Applicative, Monad, MonadState [String])

-- |
-- Run a computation in the code generation monad
--
runGen :: [String] -> Gen a -> a
runGen names = flip evalState names . unGen

-- |
-- Generate a fresh name
--
fresh :: Gen String
fresh = do
  (s:ss) <- get
  put ss
  return s
