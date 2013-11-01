-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen.Common.Gen
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PureScript.CodeGen.Common.Gen where

import Control.Monad.State
import Control.Applicative

newtype Gen a = Gen { unGen :: State Int a } deriving (Functor, Applicative, Monad, MonadState Int)

runGen :: Gen a -> a
runGen = flip evalState 0 . unGen

fresh :: Gen String
fresh = do
  n <- get
  modify (+ 1)
  return $ '_' : show n
