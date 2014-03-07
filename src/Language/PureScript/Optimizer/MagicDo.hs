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
  -- Desugae >>
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing ["_"] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSApp m [], JSReturn (JSApp ret []) ]
  -- Desugar >>=
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [arg] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSVariableIntroduction arg (Just (JSApp m [])), JSReturn (JSApp ret []) ]
  -- Desugar untilE
  convert (JSApp (JSApp f [arg]) []) | isEffFunc "untilE" f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSUnary Not (JSApp arg [])) (JSBlock []), JSReturn (JSObjectLiteral []) ])) []
  -- Desugar whileE
  convert (JSApp (JSApp (JSApp f [arg1]) [arg2]) []) | isEffFunc "whileE" f =
    JSApp (JSFunction Nothing [] (JSBlock [ JSWhile (JSApp arg1 []) (JSBlock [ JSApp arg2 [] ]), JSReturn (JSObjectLiteral []) ])) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (JSApp bindPoly [effDict]) | isBindPoly bindPoly && isEffDict effDict = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to return for the Eff monad
  isReturn (JSApp retPoly [effDict]) | isRetPoly retPoly && isEffDict effDict = True
  isReturn _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly (JSAccessor prop (JSAccessor "Prelude" (JSVar "_ps"))) | prop == identToJs (Op ">>=") = True
  isBindPoly (JSIndexer (JSStringLiteral ">>=") (JSAccessor "Prelude" (JSVar "_ps"))) = True
  isBindPoly _ = False
  -- Check if an expression represents the polymorphic return function
  isRetPoly (JSAccessor "$return" (JSAccessor "Prelude" (JSVar "_ps"))) = True
  isRetPoly (JSIndexer (JSStringLiteral "return") (JSAccessor "Prelude" (JSVar "_ps"))) = True
  isRetPoly _ = False
  -- Check if an expression represents a function in the Ef module
  isEffFunc name (JSAccessor name' (JSAccessor "Control_Monad_Eff" (JSVar "_ps"))) | name == name' = True
  isEffFunc _ _ = False
  -- The name of the type class dictionary for the Monad Eff instance
  effDictName = "monadEff"
  -- Check if an expression represents the Monad Eff dictionary
  isEffDict (JSApp (JSVar ident) [JSObjectLiteral []]) | ident == effDictName = True
  isEffDict (JSApp (JSAccessor prop (JSAccessor "Control_Monad_Eff" (JSVar "_ps"))) [JSObjectLiteral []]) | prop == effDictName = True
  isEffDict _ = False
  -- Remove __do function applications which remain after desugaring
  undo :: JS -> JS
  undo (JSReturn (JSApp (JSFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

-- |
-- Inline functions in the ST module
--
inlineST :: JS -> JS
inlineST = everywhere (mkT convertBlock)
  where
  -- Look for runST blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same runST, only using { read, write, modify }STRef then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (JSApp f [arg]) | isSTFunc "runST" f || isSTFunc "runSTArray" f =
    let refs = nub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in everywhere (mkT $ convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (JSApp (JSApp f [arg]) []) | isSTFunc "newSTRef" f =
    if agg then arg else JSObjectLiteral [("value", arg)]
  convert agg (JSApp (JSApp f [ref]) []) | isSTFunc "readSTRef" f =
    if agg then ref else JSAccessor "value" ref
  convert agg (JSApp (JSApp (JSApp f [ref]) [arg]) []) | isSTFunc "writeSTRef" f =
    if agg then JSAssignment ref arg else JSAssignment (JSAccessor "value" ref) arg
  convert agg (JSApp (JSApp (JSApp f [ref]) [func]) []) | isSTFunc "modifySTRef" f =
    if agg then JSAssignment ref (JSApp func [ref]) else  JSAssignment (JSAccessor "value" ref) (JSApp func [JSAccessor "value" ref])
  convert _ (JSApp (JSApp (JSApp f [arr]) [i]) []) | isSTFunc "peekSTArray" f =
    JSIndexer i arr
  convert _ (JSApp (JSApp (JSApp (JSApp f [arr]) [i]) [val]) []) | isSTFunc "pokeSTArray" f =
    JSAssignment (JSIndexer i arr) val
  convert _ other = other
  -- Check if an expression represents a function in the ST module
  isSTFunc name (JSAccessor name' (JSAccessor "Control_Monad_ST" (JSVar "_ps"))) | name == name' = True
  isSTFunc _ _ = False
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everything (++) (mkQ [] isSTRef)
    where
    isSTRef (JSVariableIntroduction ident (Just (JSApp (JSApp f [_]) []))) | isSTFunc "newSTRef" f = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to readSTRef, writeSTRef, modifySTRef
  findAllSTUsagesIn = everything (++) (mkQ [] isSTUsage)
    where
    isSTUsage (JSApp (JSApp f [ref]) []) | isSTFunc "readSTRef" f = [ref]
    isSTUsage (JSApp (JSApp (JSApp f [ref]) [_]) []) | isSTFunc "writeSTRef" f || isSTFunc "modifySTRef" f = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everything (++) (mkQ [] isVar)
    where
    isVar e@(JSVar v) | v == ref = [e]
    isVar _ = []
  -- Convert a JS value to a String if it is a JSVar
  toVar (JSVar v) = Just v
  toVar _ = Nothing
