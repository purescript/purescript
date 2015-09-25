-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer.MagicDo
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.JS.Optimizer.MagicDo (
  magicDo
) where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Names
import Language.PureScript.Options
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
  -- Desugar return
  convert (JSApp (JSApp ret [val]) []) | isReturn ret = val
  -- Desugar pure
  convert (JSApp (JSApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [] (JSBlock js)]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock (JSApp m [] : map applyReturns js )
  -- Desugar >>=
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [arg] (JSBlock js)]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock (JSVariableIntroduction arg (Just (JSApp m [])) : map applyReturns js)
  -- Desugar untilE
  convert (JSApp (JSApp f [arg]) []) | isEffFunc C.untilE f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSUnary Not (JSApp arg [])) (JSBlock []), JSReturn $ JSObjectLiteral []])) []
  -- Desugar whileE
  convert (JSApp (JSApp (JSApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSApp arg1 []) (JSBlock [ JSApp arg2 [] ]), JSReturn $ JSObjectLiteral []])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (JSApp bindPoly [effDict]) | isBindPoly bindPoly && isEffDict C.bindEffDictionary effDict = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to return for the Eff monad
  isReturn (JSApp retPoly [effDict]) | isRetPoly retPoly && isEffDict C.monadEffDictionary effDict = True
  isReturn _ = False
  -- Check if an expression represents a monomorphic call to pure for the Eff applicative
  isPure (JSApp purePoly [effDict]) | isPurePoly purePoly && isEffDict C.applicativeEffDictionary effDict = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly (JSAccessor prop (JSVar prelude)) = prelude == C.prelude && (prop `elem` map identToJs [Ident C.bind, Op (C.>>=)])
  isBindPoly (JSIndexer (JSStringLiteral bind) (JSVar prelude)) = prelude == C.prelude && (bind `elem` [C.bind, (C.>>=)])
  isBindPoly _ = False
  -- Check if an expression represents the polymorphic return function
  isRetPoly (JSAccessor returnEscaped (JSVar prelude)) = prelude == C.prelude && returnEscaped == C.returnEscaped
  isRetPoly (JSIndexer (JSStringLiteral return') (JSVar prelude)) = prelude == C.prelude && return' == C.return
  isRetPoly _ = False
  -- Check if an expression represents the polymorphic pure function
  isPurePoly (JSAccessor pure' (JSVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly (JSIndexer (JSStringLiteral pure') (JSVar prelude)) = prelude == C.prelude && pure' == C.pure'
  isPurePoly _ = False
  -- Check if an expression represents a function in the Ef module
  isEffFunc name (JSAccessor name' (JSVar eff)) = eff == C.eff && name == name'
  isEffFunc _ _ = False
  -- Check if an expression represents the Monad Eff dictionary
  isEffDict name (JSVar ident) | ident == name = True
  isEffDict name (JSAccessor prop (JSVar eff)) = eff == C.eff && prop == name
  isEffDict _ _ = False
  -- Remove __do function applications which remain after desugaring
  undo :: JS -> JS
  undo (JSReturn (JSApp (JSFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

  applyReturns :: JS -> JS
  applyReturns (JSReturn ret) = JSReturn (JSApp ret [])
  applyReturns (JSBlock jss) = JSBlock (map applyReturns jss)
  applyReturns (JSWhile cond js) = JSWhile cond (applyReturns js)
  applyReturns (JSFor v lo hi js) = JSFor v lo hi (applyReturns js)
  applyReturns (JSForIn v xs js) = JSForIn v xs (applyReturns js)
  applyReturns (JSIfElse cond t f) = JSIfElse cond (applyReturns t) (applyReturns `fmap` f)
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
  convertBlock (JSApp f [arg]) | isSTFunc C.runST f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhereOnJS (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (JSApp f [arg]) | isSTFunc C.newSTRef f =
   JSFunction Nothing [] (JSBlock [JSReturn $ if agg then arg else JSObjectLiteral [(C.stRefValue, arg)]])
  convert agg (JSApp (JSApp f [ref]) []) | isSTFunc C.readSTRef f =
    if agg then ref else JSAccessor C.stRefValue ref
  convert agg (JSApp (JSApp (JSApp f [ref]) [arg]) []) | isSTFunc C.writeSTRef f =
    if agg then JSAssignment ref arg else JSAssignment (JSAccessor C.stRefValue ref) arg
  convert agg (JSApp (JSApp (JSApp f [ref]) [func]) []) | isSTFunc C.modifySTRef f =
    if agg then JSAssignment ref (JSApp func [ref]) else  JSAssignment (JSAccessor C.stRefValue ref) (JSApp func [JSAccessor C.stRefValue ref])
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (JSAccessor name' (JSVar st)) = st == C.st && name == name'
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everythingOnJS (++) isSTRef
    where
    isSTRef (JSVariableIntroduction ident (Just (JSApp (JSApp f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everythingOnJS (++) isSTUsage
    where
    isSTUsage (JSApp (JSApp f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (JSApp (JSApp (JSApp f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everythingOnJS (++) isVar
    where
    isVar e@(JSVar v) | v == ref = [e]
    isVar _ = []
  -- Convert a JS value to a String if it is a JSVar
  toVar (JSVar v) = Just v
  toVar _ = Nothing
