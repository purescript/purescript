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
import Data.Maybe (fromMaybe)
import Data.Generics

import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import Language.PureScript.CodeGen.Common (identToJs)
import Language.PureScript.Sugar.TypeClasses
       (mkDictionaryValueName)
import Language.PureScript.Types

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
untilFixedPoint f a = go a
  where
  go a' = let a'' = f a' in
          if a'' == a' then a'' else go a''

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
  check _ = False

isRebound :: (Data d) => JS -> d -> Bool
isRebound js d = any (\var -> isReassigned var d) (everything (++) (mkQ [] variablesOf) js)
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

inlineVariables :: JS -> JS
inlineVariables = everywhere (mkT removeFromBlock)
  where
  removeFromBlock :: JS -> JS
  removeFromBlock (JSBlock sts) = JSBlock (go sts)
  removeFromBlock js = js
  go :: [JS] -> [JS]
  go [] = []
  go (s@(JSVariableIntroduction var (Just js)) : sts)
    | shouldInline js && not (isReassigned var sts) && not (isRebound js sts) && not (isUpdated var sts) =
      go (replaceIdent var js sts)
  go (s:sts) = s : go sts

removeUnusedVariables :: JS -> JS
removeUnusedVariables = everywhere (mkT removeFromBlock)
  where
  removeFromBlock :: JS -> JS
  removeFromBlock (JSBlock sts) = JSBlock (go sts)
  removeFromBlock js = js
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
      not (any (flip isRebound block) (map JSVar idents)) &&
      not (or (map (flip isRebound block) args))
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
  convert js@(JSVariableIntroduction name (Just fn@(JSFunction _ _ _))) =
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
    (args : allArgs, body, \b -> f (JSFunction ident (map copyVar args) b))
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    (args : allArgs, body, \b -> f (JSReturn (JSFunction ident (map copyVar args) b)))
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
magicDo opts | optionsMagicDo opts = magicDo'
             | otherwise = id

magicDo' :: JS -> JS
magicDo' = everywhere (mkT undo) . everywhere' (mkT convert)
  where
  fnName = "__do"
  convert :: JS -> JS
  convert (JSApp (JSApp ret [val]) []) | isReturn ret = val
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing ["_"] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSApp m [], JSReturn (JSApp ret []) ]
  convert (JSApp (JSApp bind [m]) [JSFunction Nothing [arg] (JSBlock [JSReturn ret])]) | isBind bind =
    JSFunction (Just fnName) [] $ JSBlock [ JSVariableIntroduction arg (Just (JSApp m [])), JSReturn (JSApp ret []) ]
  convert other = other
  isBind (JSApp bindPoly [effDict]) | isBindPoly bindPoly && isEffDict effDict = True
  isBind _ = False
  isReturn (JSApp retPoly [effDict]) | isRetPoly retPoly && isEffDict effDict = True
  isReturn _ = False
  isBindPoly (JSAccessor prop (JSVar "Prelude")) | prop == identToJs (Op ">>=") = True
  isBindPoly (JSIndexer (JSStringLiteral ">>=") (JSVar "Prelude")) = True
  isBindPoly _ = False
  isRetPoly (JSAccessor "$return" (JSVar "Prelude")) = True
  isRetPoly _ = False
  prelude = ModuleName (ProperName "Prelude")
  effModule = ModuleName (ProperName "Eff")
  Right (Ident effDictName) = mkDictionaryValueName
    effModule
    (Qualified (Just prelude) (ProperName "Monad"))
    (TypeConstructor (Qualified (Just effModule) (ProperName "Eff")))
  isEffDict (JSVar ident) | ident == effDictName = True
  isEffDict (JSAccessor prop (JSVar "Eff")) | prop == effDictName = True
  isEffDict _ = False
  undo :: JS -> JS
  undo (JSReturn (JSApp (JSFunction (Just ident) [] body) [])) | ident == fnName = body
  undo other = other

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
  convert (JSApp (JSApp op [x]) [y]) | isOp op = f x y
  convert other = other
  isOp (JSAccessor longForm (JSVar "Prelude")) | longForm == identToJs (Op op) = True
  isOp (JSIndexer (JSStringLiteral op') (JSVar "Prelude")) | op == op' = True
  isOp _ = False

inlineCommonOperators :: JS -> JS
inlineCommonOperators = applyAll
  [ binary "+" "Num" tyNumber Add
  , binary "-" "Num" tyNumber Subtract
  , binary "*" "Num" tyNumber Multiply
  , binary "/" "Num" tyNumber Divide
  , binary "%" "Num" tyNumber Modulus
  , unary "negate" "Num" tyNumber Negate

  , binary "<" "Ord" tyNumber LessThan
  , binary ">" "Ord" tyNumber GreaterThan
  , binary "<=" "Ord" tyNumber LessThanOrEqualTo
  , binary ">=" "Ord" tyNumber GreaterThanOrEqualTo

  , binary "==" "Eq" tyNumber EqualTo
  , binary "/=" "Eq" tyNumber NotEqualTo
  , binary "==" "Eq" tyString EqualTo
  , binary "/=" "Eq" tyString NotEqualTo
  , binary "==" "Eq" tyBoolean EqualTo
  , binary "/=" "Eq" tyBoolean NotEqualTo

  , binaryFunction "shl" "Bits" tyNumber ShiftLeft
  , binaryFunction "shr" "Bits" tyNumber ShiftRight
  , binaryFunction "zshr" "Bits" tyNumber ZeroFillShiftRight
  , binary "&" "Bits" tyNumber BitwiseAnd
  , binary "|" "Bits" tyNumber BitwiseOr
  , binary "^" "Bits" tyNumber BitwiseXor
  , unary "complement" "Bits" tyNumber BitwiseNot

  , binary "&&" "BoolLike" tyBoolean And
  , binary "||" "BoolLike" tyBoolean Or
  , unary "not" "BoolLike" tyBoolean Not
  ]
  where
  binary :: String -> String -> Type -> BinaryOperator -> JS -> JS
  binary opString className classTy op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isOp fn && isOpDict className classTy dict = JSBinary op x y
    convert other = other
    isOp (JSAccessor longForm (JSVar "Prelude")) | longForm == identToJs (Op opString) = True
    isOp (JSIndexer (JSStringLiteral op') (JSVar "Prelude")) | opString == op' = True
    isOp _ = False
  binaryFunction :: String -> String -> Type -> BinaryOperator -> JS -> JS
  binaryFunction fnName className classTy op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isOp fn && isOpDict className classTy dict = JSBinary op x y
    convert other = other
    isOp (JSAccessor fnName' (JSVar "Prelude")) | fnName == fnName' = True
    isOp _ = False
  unary :: String -> String -> Type -> UnaryOperator -> JS -> JS
  unary fnName className classTy op = everywhere (mkT convert)
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [dict]) [x]) | isOp fn && isOpDict className classTy dict = JSUnary op x
    convert other = other
    isOp (JSAccessor fnName' (JSVar "Prelude")) | fnName' == fnName = True
    isOp _ = False
  isOpDict className ty (JSAccessor prop (JSVar "Prelude")) | prop == dictName = True
    where
    Right (Ident dictName) = mkDictionaryValueName
      (ModuleName (ProperName "Prelude"))
      (Qualified (Just (ModuleName (ProperName "Prelude"))) (ProperName className))
      ty
  isOpDict _ _ _ = False
