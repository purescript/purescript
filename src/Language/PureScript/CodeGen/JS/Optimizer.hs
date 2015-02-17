-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module optimizes code in the simplified-Javascript intermediate representation.
--
-- The following optimizations are supported:
--
--  * Collapsing nested blocks
--
--  * Tail call elimination
--
--  * Inlining of (>>=) and ret for the Eff monad
--
--  * Removal of unnecessary thunks
--
--  * Eta conversion
--
--  * Inlining variables
--
--  * Inline Prelude.($), Prelude.(#), Prelude.(++), Prelude.(!!)
--
--  * Inlining primitive Javascript operators
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.CodeGen.JS.Optimizer (
    optimize
) where

import Control.Monad.Reader (MonadReader, ask, asks)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

import Language.PureScript.CodeGen.JS.Optimizer.Common
import Language.PureScript.CodeGen.JS.Optimizer.TCO
import Language.PureScript.CodeGen.JS.Optimizer.MagicDo
import Language.PureScript.CodeGen.JS.Optimizer.Inliner
import Language.PureScript.CodeGen.JS.Optimizer.Unused
import Language.PureScript.CodeGen.JS.Optimizer.Blocks

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: (Monad m, MonadReader (Options mode) m) => JS -> m JS
optimize js = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return js else optimize' js

optimize' :: (Monad m, MonadReader (Options mode) m) => JS -> m JS
optimize' js = do
  opts <- ask
  return $ untilFixedPoint (applyAll
    [ collapseNestedBlocks
    , collapseNestedIfs
    , tco opts
    , magicDo opts
    , removeCodeAfterReturnStatements
    , removeUnusedArg
    , removeUndefinedApp
    , unThunk
    , etaConvert
    , evaluateIifes
    , inlineVariables
    , inlineValues
    , inlineOperator (C.prelude, (C.$)) $ \f x -> JSApp f [x]
    , inlineOperator (C.prelude, (C.#)) $ \x f -> JSApp f [x]
    , inlineOperator (C.preludeUnsafe, C.unsafeIndex) $ flip JSIndexer
    , inlineCommonOperators ]) js

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f = go
  where
  go a = let a' = f a in
          if a' == a then a' else go a'
