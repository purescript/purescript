-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.CodeGen.JS.Optimizer.MagicDo (magicDo) where

import Prelude.Compat

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Optimizer.Common
import Language.PureScript.Options
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.Constants as C

magicDo :: Options -> JS -> JS
magicDo opts | optionsNoMagicDo opts = id
             | otherwise = inlineST . magicDo'

-- |
-- Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
--
magicDo' :: JS -> JS
magicDo' = everywhereOnJS undo . everywhereOnJSTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: JS -> JS
  -- Desugar pure & return
  convert (JSApp _ (JSApp _ pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (JSApp _ (JSApp _ bind [m]) [JSFunction s1 Nothing [] (JSBlock s2 js)]) | isBind bind =
    JSFunction s1 (Just fnName) [] $ JSBlock s2 (JSApp s2 m [] : map applyReturns js )
  -- Desugar >>=
  convert (JSApp _ (JSApp _ bind [m]) [JSFunction s1 Nothing [arg] (JSBlock s2 js)]) | isBind bind =
    JSFunction s1 (Just fnName) [] $ JSBlock s2 (JSVariableIntroduction s2 arg (Just (JSApp s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert (JSApp s1 (JSApp _ f [arg]) []) | isEffFunc C.untilE f =
    JSApp s1 (JSFunction s1 Nothing [] (JSBlock s1 [ JSWhile s1 (JSUnary s1 Not (JSApp s1 arg [])) (JSBlock s1 []), JSReturn s1 $ JSObjectLiteral s1 []])) []
  -- Desugar whileE
  convert (JSApp _ (JSApp _ (JSApp s1 f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    JSApp s1 (JSFunction s1 Nothing [] (JSBlock s1 [ JSWhile s1 (JSApp s1 arg1 []) (JSBlock s1 [ JSApp s1 arg2 [] ]), JSReturn s1 $ JSObjectLiteral s1 []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (JSApp _ fn [dict]) | isDict (C.eff, C.bindEffDictionary) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (JSApp _ fn [dict]) | isDict (C.eff, C.applicativeEffDictionary) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents a function in the Eff module
  isEffFunc name (JSIndexer _ (JSStringLiteral _ name') (JSVar _ eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False

  -- Remove __do function applications which remain after desugaring
  undo :: JS -> JS
  undo (JSReturn _ (JSApp _ (JSFunction _ (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

  applyReturns :: JS -> JS
  applyReturns (JSReturn ss ret) = JSReturn ss (JSApp ss ret [])
  applyReturns (JSBlock ss jss) = JSBlock ss (map applyReturns jss)
  applyReturns (JSWhile ss cond js) = JSWhile ss cond (applyReturns js)
  applyReturns (JSFor ss v lo hi js) = JSFor ss v lo hi (applyReturns js)
  applyReturns (JSForIn ss v xs js) = JSForIn ss v xs (applyReturns js)
  applyReturns (JSIfElse ss cond t f) = JSIfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- |
-- Inline functions in the ST module
--
inlineST :: JS -> JS
inlineST = everywhereOnJS convertBlock
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (JSApp _ f [arg]) | isSTFunc C.runST f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhereOnJS (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (JSApp s1 f [arg]) | isSTFunc C.newSTRef f =
   JSFunction s1 Nothing [] (JSBlock s1 [JSReturn s1 $ if agg then arg else JSObjectLiteral s1 [(mkString C.stRefValue, arg)]])
  convert agg (JSApp _ (JSApp s1 f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else JSIndexer s1 (JSStringLiteral s1 C.stRefValue) ref
  convert agg (JSApp _ (JSApp _ (JSApp s1 f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then JSAssignment s1 ref arg else JSAssignment s1 (JSIndexer s1 (JSStringLiteral s1 C.stRefValue) ref) arg
  convert agg (JSApp _ (JSApp _ (JSApp s1 f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then JSAssignment s1 ref (JSApp s1 func [ref]) else JSAssignment s1 (JSIndexer s1 (JSStringLiteral s1 C.stRefValue) ref) (JSApp s1 func [JSIndexer s1 (JSStringLiteral s1 C.stRefValue) ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (JSIndexer _ (JSStringLiteral _ name') (JSVar _ st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnJS (++) isSTRef
    where
    isSTRef (JSVariableIntroduction _ ident (Just (JSApp _ (JSApp _ f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everythingOnJS (++) isSTUsage
    where
    isSTUsage (JSApp _ (JSApp _ f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (JSApp _ (JSApp _ (JSApp _ f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everythingOnJS (++) isVar
    where
    isVar e@(JSVar _ v) | v == ref = [e]
    isVar _ = []
  -- Convert a JS value to a String if it is a JSVar
  toVar (JSVar _ v) = Just v
  toVar _ = Nothing
