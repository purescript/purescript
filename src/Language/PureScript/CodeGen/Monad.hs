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
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.CodeGen.Monad where

import Control.Monad.State
import Control.Applicative
import Language.PureScript.Names

newtype Gen a = Gen { unGen :: State [Ident] a } deriving (Functor, Applicative, Monad, MonadState [Ident])

runGen :: [Ident] -> Gen a -> a
runGen names = flip evalState names . unGen

fresh :: Gen Ident
fresh = do
  (s:ss) <- get
  put ss
  return s
