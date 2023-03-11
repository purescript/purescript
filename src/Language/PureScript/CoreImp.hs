-- | The imperative core language
module Language.PureScript.CoreImp (
  module C
) where

import Language.PureScript.CoreImp.AST as C
    ( everything,
      everywhere,
      everywhereTopDown,
      everywhereTopDownM,
      getSourceSpan,
      withSourceSpan,
      AST(..),
      BinaryOperator(..),
      CIComments(..),
      InitializerEffects(..),
      UnaryOperator(..) )
import Language.PureScript.CoreImp.Optimizer as C ( optimize )
import Language.PureScript.CoreImp.Optimizer.Blocks as C
    ( collapseNestedBlocks, collapseNestedIfs )
import Language.PureScript.CoreImp.Optimizer.Common as C
    ( pattern Ref,
      applyAll,
      isReassigned,
      isRebound,
      isUpdated,
      refPatternHelper,
      removeFromBlock,
      replaceIdent,
      replaceIdents,
      targetVariable )
import Language.PureScript.CoreImp.Optimizer.Inliner as C
    ( etaConvert,
      evaluateIifes,
      inlineCommonOperators,
      inlineCommonValues,
      inlineFnComposition,
      inlineFnIdentity,
      inlineUnsafeCoerce,
      inlineUnsafePartial,
      inlineVariables,
      unThunk )
import Language.PureScript.CoreImp.Optimizer.MagicDo as C
    ( inlineST, magicDoEff, magicDoEffect, magicDoST )
import Language.PureScript.CoreImp.Optimizer.TCO as C ( tco )
import Language.PureScript.CoreImp.Optimizer.Unused as C
    ( removeCodeAfterReturnStatements,
      removeUndefinedApp,
      removeUnusedEffectFreeVars )
