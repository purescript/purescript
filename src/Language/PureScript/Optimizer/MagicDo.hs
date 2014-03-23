-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.MagicDo
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

module Language.PureScript.Optimizer.MagicDo (
  magicDo
) where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import Data.Generics

import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.Common (identToJs)
import Language.PureScript.Names

import qualified Language.PureScript.Constants as C

magicDo :: Options -> JS -> JS
magicDo opts | optionsMagicDo opts = inlineST . magicDo'
             | otherwise = id

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
magicDo' = everywhere (mkT undo) . everywhere' (mkT convert)
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: JS -> JS
  -- Desugar return
  convert (JSApp (JSApp ret [val]) []) | isReturn ret = val
  -- Desugar >>
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing ["_"] (JSBlock js)]) | isBind bind && isJSReturn (last js) =
    let JSReturn ret = last js in
    JSFunction (Just fnName) [] $ JSBlock (JSApp m [] : init js ++ [JSReturn (JSApp ret [])] )
  -- Desugar >>=
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [arg] (JSBlock js)]) | isBind bind && isJSReturn (last js) =
    let JSReturn ret = last js in
    JSFunction (Just fnName) [] $ JSBlock (JSVariableIntroduction arg (Just (JSApp m [])) : init js ++ [JSReturn (JSApp ret [])] )
  -- Desugar untilE
  convert (JSApp (JSApp f [arg]) []) | isEffFunc C.untilE f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSUnary Not (JSApp arg [])) (JSBlock []), JSReturn (JSObjectLiteral []) ])) []
  -- Desugar whileE
  convert (JSApp (JSApp (JSApp f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSApp arg1 []) (JSBlock [ JSApp arg2 [] ]), JSReturn (JSObjectLiteral []) ])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (JSApp bindPoly [effDict]) | isBindPoly bindPoly && isEffDict effDict = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to return for the Eff monad
  isReturn (JSApp retPoly [effDict]) | isRetPoly retPoly && isEffDict effDict = True
  isReturn _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly (JSAccessor prop (JSAccessor prelude (JSVar _ps))) | prelude == C.prelude &&
                                                                  _ps == C._ps &&
                                                                  prop == identToJs (Op (C.>>=)) = True
  isBindPoly (JSIndexer (JSStringLiteral bind) (JSAccessor prelude (JSVar _ps))) | prelude == C.prelude &&
                                                                                   _ps == C._ps &&
                                                                                   bind == (C.>>=) = True
  isBindPoly _ = False
  -- Check if an expression represents the polymorphic return function
  isRetPoly (JSAccessor returnEscaped (JSAccessor prelude (JSVar _ps))) | prelude == C.prelude &&
                                                                          _ps == C._ps &&
                                                                          returnEscaped == C.returnEscaped = True
  isRetPoly (JSIndexer (JSStringLiteral return') (JSAccessor prelude (JSVar _ps))) | prelude == C.prelude &&
                                                                                    _ps == C._ps &&
                                                                                    return' == C.return = True
  isRetPoly _ = False
  -- Check if an expression represents a function in the Ef module
  isEffFunc name (JSAccessor name' (JSAccessor eff (JSVar _ps))) | eff == C.eff &&
                                                                   _ps == C._ps &&
                                                                   name == name' = True
  isEffFunc _ _ = False
  -- Check if an expression represents the Monad Eff dictionary
  isEffDict (JSApp (JSVar ident) [JSObjectLiteral []]) | ident == C.monadEffDictionary = True
  isEffDict (JSApp (JSAccessor prop (JSAccessor eff (JSVar _ps))) [JSObjectLiteral []]) | eff == C.eff &&
                                                                                          _ps == C._ps &&
                                                                                          prop == C.monadEffDictionary = True
  isEffDict _ = False
  -- Remove __do function applications which remain after desugaring
  undo :: JS -> JS
  undo (JSReturn (JSApp (JSFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

  isJSReturn (JSReturn _) = True
  isJSReturn _ = False

-- |
-- Inline functions in the ST module
--
inlineST :: JS -> JS
inlineST = everywhere (mkT convertBlock)
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (JSApp f [arg]) | isSTFunc C.runST f || isSTFunc C.runSTArray f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhere (mkT $ convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
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
  convert _ (JSApp (JSApp (JSApp f [arr]) [i]) []) | isSTFunc C.peekSTArray f =
    JSIndexer i arr
  convert _ (JSApp (JSApp (JSApp (JSApp f [arr]) [i]) [val]) []) | isSTFunc C.pokeSTArray f =
    JSAssignment (JSIndexer i arr) val
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (JSAccessor name' (JSAccessor st (JSVar _ps))) | st == C.st &&
                                                                   _ps == C._ps &&
                                                                   name == name' = True
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everything (++) (mkQ [] isSTRef)
    where
    isSTRef (JSVariableIntroduction ident (Just (JSApp (JSApp f [_]) []))) | isSTFunc C.newSTRef f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everything (++) (mkQ [] isSTUsage)
    where
    isSTUsage (JSApp (JSApp f [ref]) []) | isSTFunc C.readSTRef f = [ref]
    isSTUsage (JSApp (JSApp (JSApp f [ref]) [_]) []) | isSTFunc C.writeSTRef f || isSTFunc C.modifySTRef f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everything (++) (mkQ [] isVar)
    where
    isVar e@(JSVar v) | v == ref = [e]
    isVar _ = []
  -- Convert a JS value to a String if it is a JSVar
  toVar (JSVar v) = Just v
  toVar _ = Nothing
