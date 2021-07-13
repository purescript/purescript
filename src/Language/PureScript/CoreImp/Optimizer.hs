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

import Prelude.Compat

import Data.Text (Text)

import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Blocks
import Language.PureScript.CoreImp.Optimizer.Common
import Language.PureScript.CoreImp.Optimizer.Inliner
import Language.PureScript.CoreImp.Optimizer.MagicDo
import Language.PureScript.CoreImp.Optimizer.TCO
import Language.PureScript.CoreImp.Optimizer.Unused

-- | Apply a series of optimizer passes to simplified JavaScript code
optimize :: forall m. MonadSupply m => [Text] -> [[AST]] -> m [[AST]]
optimize exps jsDecls = removeUnusedPureVars exps <$> traverse (traverse go) jsDecls
  where
    go :: AST -> m AST
    go js = do
      js' <- untilFixedPoint (inlineFnComposition expander . inlineFnIdentity expander . inlineUnsafeCoerce . inlineUnsafePartial . tidyUp . applyAll
        [ inlineCommonValues expander
        , inlineCommonOperators expander
        ]) js
      untilFixedPoint (return . tidyUp) . tco . inlineST
        =<< untilFixedPoint (return . magicDoST expander)
        =<< untilFixedPoint (return . magicDoEff expander)
        =<< untilFixedPoint (return . magicDoEffect expander) js'

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

    expander = buildExpander (concat jsDecls)

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'

-- |
-- Take all top-level ASTs and return a function for expanding top-level
-- variables during the various inlining steps in `optimize`.
--
-- Everything that gets inlined as an optimization is of a form that would
-- have been lifted to a top-level binding during CSE, so for purposes of
-- inlining we can save some time by only expanding variables bound at that
-- level and not worrying about any inner scopes.
--
buildExpander :: [AST] -> AST -> AST
buildExpander = replaceIdents . foldr go []
  where
  go = \case
    VariableIntroduction _ name (Just (IsPure, e)) -> ((name, e) :)
    _ -> id
