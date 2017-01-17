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
module Language.PureScript.CodeGen.JS.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Optimizer.Blocks
import Language.PureScript.CodeGen.JS.Optimizer.Common
import Language.PureScript.CodeGen.JS.Optimizer.Inliner
import Language.PureScript.CodeGen.JS.Optimizer.MagicDo
import Language.PureScript.CodeGen.JS.Optimizer.TCO
import Language.PureScript.CodeGen.JS.Optimizer.Unused
import Language.PureScript.Options

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: (MonadReader Options m, MonadSupply m) => JS -> m JS
optimize js = do
  noOpt <- asks optionsNoOptimizations
  if noOpt then return js else optimize' js

optimize' :: (MonadReader Options m, MonadSupply m) => JS -> m JS
optimize' js = do
  opts <- ask
  js' <- untilFixedPoint (inlineFnComposition . inlineUnsafePartial . tidyUp . applyAll
    [ inlineCommonValues
    , inlineCommonOperators
    ]) js
  untilFixedPoint (return . tidyUp) . tco opts . magicDo opts $ js'
  where
  tidyUp :: JS -> JS
  tidyUp = applyAll
    [ collapseNestedBlocks
    , collapseNestedIfs
    , removeCodeAfterReturnStatements
    , removeUnusedArg
    , removeUndefinedApp
    , unThunk
    , etaConvert
    , evaluateIifes
    , inlineVariables
    ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
