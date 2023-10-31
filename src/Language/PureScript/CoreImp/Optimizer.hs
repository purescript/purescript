-- | This module optimizes code in the simplified-JavaScript intermediate representation.
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
--  * Inlining primitive JavaScript operators
module Language.PureScript.CoreImp.Optimizer (optimize) where

import Prelude

import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CoreImp.AST (AST(..))
import Language.PureScript.CoreImp.Optimizer.Blocks (collapseNestedBlocks, collapseNestedIfs)
import Language.PureScript.CoreImp.Optimizer.Common (applyAll, replaceIdents)
import Language.PureScript.CoreImp.Optimizer.Inliner (etaConvert, evaluateIifes, inlineCommonOperators, inlineCommonValues, inlineFnComposition, inlineFnIdentity, inlineUnsafeCoerce, inlineUnsafePartial, inlineVariables, unThunk)
import Language.PureScript.CoreImp.Optimizer.MagicDo (inlineST, magicDoEff, magicDoEffect, magicDoST)
import Language.PureScript.CoreImp.Optimizer.TCO (tco)
import Language.PureScript.CoreImp.Optimizer.Unused (removeCodeAfterReturnStatements, removeUndefinedApp)

-- | Apply a series of optimizer passes to simplified JavaScript code
optimize :: MonadSupply m => AST -> m AST
optimize js = do
    js' <- untilFixedPoint (inlineFnComposition . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp . applyAll
      [ inlineCommonValues
      , inlineCommonOperators
      ]) js
    untilFixedPoint (return . tidyUp) . tco . inlineST
      =<< untilFixedPoint (return . magicDoST)
      =<< untilFixedPoint (return . magicDoEff)
      =<< untilFixedPoint (return . magicDoEffect) js'
  where
    tidyUp :: AST -> AST
    tidyUp = applyAll
      [ collapseNestedBlocks
      , collapseNestedIfs
      , removeCodeAfterReturnStatements
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
