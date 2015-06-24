-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CoreImp.Optimizer (optimize) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.Core
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Nesting
import Language.PureScript.CoreImp.Optimizer.Unused
import Language.PureScript.Options

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: forall m mode. (Monad m, MonadReader (Options mode) m,  Applicative m, MonadSupply m) =>
              Module (Decl Ann) -> m (Module (Decl Ann))
optimize m = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return m else optimize' m
  where
  optimize' :: Module (Decl Ann) -> m (Module (Decl Ann))
  optimize' (Module coms mn imps exps externs decls) =
    return $ Module coms mn imps exps externs $ untilFixedPoint (applyAll
      [ collapseNestedIfs
      , removeCodeAfterReturnStatements
      , removeUnusedArg
      , removeUndefinedApp
      ]) `map` decls

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f = go
  where
  go a = let a' = f a in
          if a' == a then a' else go a'

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)
