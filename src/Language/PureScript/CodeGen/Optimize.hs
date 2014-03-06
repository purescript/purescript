-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimize
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
--  * Removal of unused variables
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

module Language.PureScript.CodeGen.Optimize (
    optimize
) where

import Data.Data
import Data.List (nub)
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Generics

import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import Language.PureScript.CodeGen.Common (identToJs)

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: Options -> JS -> JS
optimize opts | optionsNoOptimizations opts = id
              | otherwise = untilFixedPoint $ applyAll
  [ collapseNestedBlocks
  , tco opts
  , magicDo opts
  , removeUnusedVariables
  , unThunk
  , etaConvert
  , inlineVariables
  , inlineOperator "$" $ \f x -> JSApp f [x]
  , inlineOperator "#" $ \x f -> JSApp f [x]
  , inlineOperator "!!" $ flip JSIndexer
  , inlineOperator "++" $ JSBinary Add
  , inlineCommonOperators ]

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

untilFixedPoint :: (Eq a) => (a -> a) -> a -> a
untilFixedPoint f = go
  where
  go a = let a' = f a in
          if a' == a then a' else go a'

replaceIdent :: (Data d) => String -> JS -> d -> d
replaceIdent var1 js = everywhere (mkT replace)
  where
  replace (JSVar var2) | var1 == var2 = js
  replace other = other

replaceIdents :: (Data d) => [(String, JS)] -> d -> d
replaceIdents vars = everywhere (mkT replace)
  where
  replace v@(JSVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: (Data d) => String -> d -> Bool
isReassigned var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSFunction _ args _) | var1 `elem` args = True
  check (JSVariableIntroduction arg _) | var1 == arg = True
  check (JSAssignment (JSVar arg) _) | var1 == arg = True
  check _ = False

isRebound :: (Data d) => JS -> d -> Bool
isRebound js d = any (`isReassigned` d) (everything (++) (mkQ [] variablesOf) js)
  where
  variablesOf (JSVar var) = [var]
  variablesOf _ = []

isUsed :: (Data d) => String -> d -> Bool
isUsed var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSVar var2) | var1 == var2 = True
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: JS -> String
targetVariable (JSVar var) = var
targetVariable (JSAccessor _ tgt) = targetVariable tgt
targetVariable (JSIndexer _ tgt) = targetVariable tgt
targetVariable _ = error "Invalid argument to targetVariable"

isUpdated :: (Data d) => String -> d -> Bool
isUpdated var1 = everything (||) (mkQ False check)
  where
  check :: JS -> Bool
  check (JSAssignment target _) | var1 == targetVariable target = True
  check _ = False

shouldInline :: JS -> Bool
shouldInline (JSVar _) = True
shouldInline (JSNumericLiteral _) = True
shouldInline (JSStringLiteral _) = True
shouldInline (JSBooleanLiteral _) = True
shouldInline (JSAccessor _ val) = shouldInline val
shouldInline (JSIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

removeFromBlock :: ([JS] -> [JS]) -> JS -> JS
removeFromBlock go (JSBlock sts) = JSBlock (go sts)
removeFromBlock _  js = js

inlineVariables :: JS -> JS
inlineVariables = everywhere (mkT $ removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var (Just js) : sts)
    | shouldInline js && not (isReassigned var sts) && not (isRebound js sts) && not (isUpdated var sts) =
      go (replaceIdent var js sts)
  go (s:sts) = s : go sts

removeUnusedVariables :: JS -> JS
removeUnusedVariables = everywhere (mkT $ removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var _ : sts) | not (isUsed var sts) = go sts
  go (s:sts) = s : go sts

etaConvert :: JS -> JS
etaConvert = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing idents block@(JSBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map JSVar idents)) &&
      not (any (`isRebound` block) args)
      = JSBlock (replaceIdents (zip idents args) body)
  convert js = js

unThunk :: JS -> JS
unThunk = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing [] (JSBlock body)) [])]) = JSBlock body
  convert js = js

tco :: Options -> JS -> JS
tco opts | optionsTco opts = tco'
         | otherwise = id

tco' :: JS -> JS
tco' = everywhere (mkT convert)
  where
  tcoLabel :: String
  tcoLabel = "tco"
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg
  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg
  convert :: JS -> JS
  convert js@(JSVariableIntroduction name (Just fn@JSFunction {})) =
    let
      (argss, body', replace) = collectAllFunctionArgs [] id fn
    in case () of
      _ | isTailCall name body' ->
            let
              allArgs = reverse $ concat argss
            in
              JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
        | otherwise -> js
  convert js = js
  collectAllFunctionArgs :: [[String]] -> (JS -> JS) -> JS -> ([[String]], JS, JS -> JS)
  collectAllFunctionArgs allArgs f (JSFunction ident args (JSBlock (body@(JSReturn _):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction ident (map copyVar args) (JSBlock [b]))) body
  collectAllFunctionArgs allArgs f (JSFunction ident args body@(JSBlock _)) =
    (args : allArgs, body, f . JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    (args : allArgs, body, f . JSReturn . JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = (allArgs, body, f)
  isTailCall :: String -> JS -> Bool
  isTailCall ident js =
    let
      numSelfCalls = everything (+) (mkQ 0 countSelfCalls) js
      numSelfCallsInTailPosition = everything (+) (mkQ 0 countSelfCallsInTailPosition) js
      numSelfCallsUnderFunctions = everything (+) (mkQ 0 countSelfCallsUnderFunctions) js
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
    where
    countSelfCalls :: JS -> Int
    countSelfCalls (JSApp (JSVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0
    countSelfCallsInTailPosition :: JS -> Int
    countSelfCallsInTailPosition (JSReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0
    countSelfCallsUnderFunctions (JSFunction _ _ js') = everything (+) (mkQ 0 countSelfCalls) js'
    countSelfCallsUnderFunctions _ = 0
  toLoop :: String -> [String] -> JS -> JS
  toLoop ident allArgs js = JSBlock $
        map (\arg -> JSVariableIntroduction arg (Just (JSVar (copyVar arg)))) allArgs ++
        [ JSLabel tcoLabel $ JSWhile (JSBooleanLiteral True) (JSBlock [ everywhere (mkT loopify) js ]) ]
    where
    loopify :: JS -> JS
    loopify (JSReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        JSBlock $ zipWith (\val arg ->
                    JSVariableIntroduction (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    JSAssignment (JSVar arg) (JSVar (tcoVar arg))) allArgs
                  ++ [ JSContinue tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[JS]] -> JS -> [[JS]]
    collectSelfCallArgs allArgumentValues (JSApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues
  isSelfCall :: String -> JS -> Bool
  isSelfCall ident (JSApp (JSVar ident') _) | ident == ident' = True
  isSelfCall ident (JSApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False

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

collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhere (mkT collapse)
  where
  collapse :: JS -> JS
  collapse (JSBlock sts) = JSBlock (concatMap go sts)
  collapse js = js
  go :: JS -> [JS]
  go (JSBlock sts) = sts
  go s = [s]

inlineOperator :: String -> (JS -> JS -> JS) -> JS -> JS
inlineOperator op f = everywhere (mkT convert)
  where
  convert :: JS -> JS
  convert (JSApp (JSApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (JSAccessor longForm (JSAccessor "Prelude" (JSVar "_ps"))) | longForm == identToJs (Op op) = True
  isOp (JSIndexer (JSStringLiteral op') (JSAccessor "Prelude" (JSVar "_ps"))) | op == op' = True
  isOp _ = False

inlineCommonOperators :: JS -> JS
inlineCommonOperators = applyAll
  [ binary "numNumber" "+" Add
  , binary "numNumber" "-" Subtract
  , binary "numNumber" "*" Multiply
  , binary "numNumber" "/" Divide
  , binary "numNumber" "%" Modulus
  , unary  "numNumber" "negate" Negate

  , binary "ordNumber" "<" LessThan
  , binary "ordNumber" ">" GreaterThan
  , binary "ordNumber" "<=" LessThanOrEqualTo
  , binary "ordNumber" ">=" GreaterThanOrEqualTo

  , binary "eqNumber" "==" EqualTo
  , binary "eqNumber" "/=" NotEqualTo
  , binary "eqString" "==" EqualTo
  , binary "eqString" "/=" NotEqualTo
  , binary "eqBoolean" "==" EqualTo
  , binary "eqBoolean" "/=" NotEqualTo

  , binaryFunction "bitsNumber" "shl" ShiftLeft
  , binaryFunction "bitsNumber" "shr" ShiftRight
  , binaryFunction "bitsNumber" "zshr" ZeroFillShiftRight
  , binary         "bitsNumber" "&" BitwiseAnd
  , binary         "bitsNumber" "|" BitwiseOr
  , binary         "bitsNumber" "^" BitwiseXor
  , unary          "bitsNumber" "complement" BitwiseNot

  , binary "boolLikeBoolean" "&&" And
  , binary "boolLikeBoolean" "||" Or
  , unary  "boolLikeBoolean" "not" Not
  ]
  where
  binary :: String -> String -> BinaryOperator -> JS -> JS
  binary dictName opString op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isOp fn && isOpDict dictName dict = JSBinary op x y
    convert other = other
    isOp (JSAccessor longForm (JSAccessor "Prelude" (JSVar _))) | longForm == identToJs (Op opString) = True
    isOp (JSIndexer (JSStringLiteral op') (JSAccessor "Prelude" (JSVar "_ps"))) | opString == op' = True
    isOp _ = False
  binaryFunction :: String -> String -> BinaryOperator -> JS -> JS
  binaryFunction dictName fnName op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isOp fn && isOpDict dictName dict = JSBinary op x y
    convert other = other
    isOp (JSAccessor fnName' (JSAccessor "Prelude" (JSVar "_ps"))) | fnName == fnName' = True
    isOp _ = False
  unary :: String -> String -> UnaryOperator -> JS -> JS
  unary dictName fnName op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [dict]) [x]) | isOp fn && isOpDict dictName dict = JSUnary op x
    convert other = other
    isOp (JSAccessor fnName' (JSAccessor "Prelude" (JSVar "_ps"))) | fnName' == fnName = True
    isOp _ = False
  isOpDict dictName (JSApp (JSAccessor prop (JSAccessor "Prelude" (JSVar "_ps"))) [JSObjectLiteral []]) | prop == dictName = True
  isOpDict _ _ = False
