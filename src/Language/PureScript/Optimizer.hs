-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer
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

module Language.PureScript.Optimizer (
    optimize
) where

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C

import Language.PureScript.Optimizer.Common
import Language.PureScript.Optimizer.TCO
import Language.PureScript.Optimizer.MagicDo
import Language.PureScript.Optimizer.Inliner
import Language.PureScript.Optimizer.Unused
import Language.PureScript.Optimizer.Blocks

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: Options mode -> JS -> JS
optimize opts | optionsNoOptimizations opts = id
              | otherwise = untilFixedPoint (applyAll
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
  , inlineOperator (C.prelude, (C.$)) $ \f x -> JSApp f [x]
  , inlineOperator (C.prelude, (C.#)) $ \x f -> JSApp f [x]
  , inlineOperator (C.preludeUnsafe, C.unsafeIndex) $ flip JSIndexer
  , inlineCommonOperators ])

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f = go
  where
  go a = let a' = f a in
          if a' == a then a' else go a'

