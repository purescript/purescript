-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.Optimizer.Inliner
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module provides basic inlining capabilities
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.JS.Optimizer.Inliner (
  inlineVariables,
  inlineValues,
  inlineOperator,
  inlineCommonOperators,
  inlineFnComposition,
  etaConvert,
  unThunk,
  evaluateIifes
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative)
#endif
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.Optimizer.Common
import qualified Language.PureScript.Constants as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: JS -> Bool
shouldInline (JSVar _) = True
shouldInline (JSNumericLiteral _) = True
shouldInline (JSStringLiteral _) = True
shouldInline (JSBooleanLiteral _) = True
shouldInline (JSAccessor _ val) = shouldInline val
shouldInline (JSIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: JS -> JS
etaConvert = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSBlock [JSReturn (JSApp (JSFunction Nothing idents block@(JSBlock body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map JSVar idents)) &&
      not (any (`isRebound` block) args)
      = JSBlock (map (replaceIdents (zip idents args)) body)
  convert (JSFunction Nothing [] (JSBlock [JSReturn (JSApp fn [])])) = fn
  convert js = js

unThunk :: JS -> JS
unThunk = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSBlock []) = JSBlock []
  convert (JSBlock jss) =
    case last jss of
      JSReturn (JSApp (JSFunction Nothing [] (JSBlock body)) []) -> JSBlock $ init jss ++ body
      _ -> JSBlock jss
  convert js = js

evaluateIifes :: JS -> JS
evaluateIifes = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSApp (JSFunction Nothing [] (JSBlock [JSReturn ret])) []) = ret
  convert js = js

inlineVariables :: JS -> JS
inlineVariables = everywhereOnJS $ removeFromBlock go
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var (Just js) : sts)
    | shouldInline js && not (any (isReassigned var) sts) && not (any (isRebound js) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var js) sts)
  go (s:sts) = s : go sts

inlineValues :: JS -> JS
inlineValues = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSApp fn [dict]) | isDict semiringNumber dict && isFn fnZero fn = JSNumericLiteral (Left 0)
                            | isDict semiringNumber dict && isFn fnOne fn = JSNumericLiteral (Left 1)
                            | isDict semiringInt dict && isFn fnZero fn = JSNumericLiteral (Left 0)
                            | isDict semiringInt dict && isFn fnOne fn = JSNumericLiteral (Left 1)
                            | isDict boundedBoolean dict && isFn fnBottom fn = JSBooleanLiteral False
                            | isDict boundedBoolean dict && isFn fnTop fn = JSBooleanLiteral True
  convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = JSBinary BitwiseOr (JSBinary Add x y) (JSNumericLiteral (Left 0))
    | isDict semiringInt dict && isFn fnMultiply fn = JSBinary BitwiseOr (JSBinary Multiply x y) (JSNumericLiteral (Left 0))
    | isDict moduloSemiringInt dict && isFn fnDivide fn = JSBinary BitwiseOr (JSBinary Divide x y) (JSNumericLiteral (Left 0))
    | isDict ringInt dict && isFn fnSubtract fn = JSBinary BitwiseOr (JSBinary Subtract x y) (JSNumericLiteral (Left 0))
  convert other = other
  fnZero = (C.prelude, C.zero)
  fnOne = (C.prelude, C.one)
  fnBottom = (C.prelude, C.bottom)
  fnTop = (C.prelude, C.top)
  fnAdd = (C.prelude, (C.+))
  fnDivide = (C.prelude, (C./))
  fnMultiply = (C.prelude, (C.*))
  fnSubtract = (C.prelude, (C.-))

inlineOperator :: (String, String) -> (JS -> JS -> JS) -> JS -> JS
inlineOperator (m, op) f = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSApp (JSApp op' [x]) [y]) | isOp op' = f x y
  convert other = other
  isOp (JSAccessor longForm (JSVar m')) = m == m' && longForm == identToJs (Op op)
  isOp (JSIndexer (JSStringLiteral op') (JSVar m')) = m == m' && op == op'
  isOp _ = False

inlineCommonOperators :: JS -> JS
inlineCommonOperators = applyAll $
  [ binary semiringNumber (C.+) Add
  , binary semiringNumber (C.*) Multiply

  , binary ringNumber (C.-) Subtract
  , unary  ringNumber C.negate Negate
  , binary ringInt (C.-) Subtract
  , unary  ringInt C.negate Negate

  , binary moduloSemiringNumber (C./) Divide
  , binary moduloSemiringInt C.mod Modulus

  , binary eqNumber (C.==) EqualTo
  , binary eqNumber (C./=) NotEqualTo
  , binary eqInt (C.==) EqualTo
  , binary eqInt (C./=) NotEqualTo
  , binary eqString (C.==) EqualTo
  , binary eqString (C./=) NotEqualTo
  , binary eqBoolean (C.==) EqualTo
  , binary eqBoolean (C./=) NotEqualTo

  , binary ordNumber (C.<) LessThan
  , binary ordNumber (C.>) GreaterThan
  , binary ordNumber (C.<=) LessThanOrEqualTo
  , binary ordNumber (C.>=) GreaterThanOrEqualTo
  , binary ordInt (C.<) LessThan
  , binary ordInt (C.>) GreaterThan
  , binary ordInt (C.<=) LessThanOrEqualTo
  , binary ordInt (C.>=) GreaterThanOrEqualTo

  , binary semigroupString (C.<>) Add
  , binary semigroupString (C.++) Add

  , binary booleanAlgebraBoolean (C.&&) And
  , binary booleanAlgebraBoolean (C.||) Or
  , binaryFunction booleanAlgebraBoolean C.conj And
  , binaryFunction booleanAlgebraBoolean C.disj Or
  , unary booleanAlgebraBoolean C.not Not

  , binary' C.dataIntBits (C..|.) BitwiseOr
  , binary' C.dataIntBits (C..&.) BitwiseAnd
  , binary' C.dataIntBits (C..^.) BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (String, String) -> String -> BinaryOperator -> JS -> JS
  binary dict opString op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) | isDict dict dict' && isPreludeFn opString fn = JSBinary op x y
    convert other = other
  binary' :: String -> String -> BinaryOperator -> JS -> JS
  binary' moduleName opString op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [x]) [y]) | isFn (moduleName, opString) fn = JSBinary op x y
    convert other = other
  binaryFunction :: (String, String) -> String -> BinaryOperator -> JS -> JS
  binaryFunction dict fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) | isPreludeFn fnName fn && isDict dict dict' = JSBinary op x y
    convert other = other
  unary :: (String, String) -> String -> UnaryOperator -> JS -> JS
  unary dict fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [dict']) [x]) | isPreludeFn fnName fn && isDict dict dict' = JSUnary op x
    convert other = other
  unary' :: String -> String -> UnaryOperator -> JS -> JS
  unary' moduleName fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp fn [x]) | isFn (moduleName, fnName) fn = JSUnary op x
    convert other = other
  mkFn :: Int -> JS -> JS
  mkFn 0 = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp mkFnN [JSFunction Nothing [_] (JSBlock js)]) | isNFn C.mkFn 0 mkFnN =
      JSFunction Nothing [] (JSBlock js)
    convert other = other
  mkFn n = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert orig@(JSApp mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, js) -> JSFunction Nothing args (JSBlock js)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [String] -> JS -> Maybe ([String], [JS])
    collectArgs 1 acc (JSFunction Nothing [oneArg] (JSBlock js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (JSFunction Nothing [oneArg] (JSBlock [JSReturn ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: String -> Int -> JS -> Bool
  isNFn prefix n (JSVar name) = name == (prefix ++ show n)
  isNFn prefix n (JSAccessor name (JSVar dataFunction)) | dataFunction == C.dataFunction = name == (prefix ++ show n)
  isNFn _ _ _ = False

  runFn :: Int -> JS -> JS
  runFn n = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [JS] -> JS -> Maybe JS
    go 0 acc (JSApp runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (JSApp fn acc)
    go m acc (JSApp lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: (Applicative m, MonadSupply m) => JS -> m JS
inlineFnComposition = everywhereOnJSTopDownM convert
  where
  convert :: (MonadSupply m) => JS -> m JS
  convert (JSApp (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) [z]) | isFnCompose dict' fn =
    return $ JSApp x [JSApp y [z]]
  convert (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) | isFnCompose dict' fn = do
    arg <- freshName
    return $ JSFunction Nothing [arg] (JSBlock [JSReturn $ JSApp x [JSApp y [JSVar arg]]])
  convert other = return other
  isFnCompose :: JS -> JS -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && (isPreludeFn (C.<<<) fn || isPreludeFn C.compose fn)

isDict :: (String, String) -> JS -> Bool
isDict (moduleName, dictName) (JSAccessor x (JSVar y)) = x == dictName && y == moduleName
isDict _ _ = False

isFn :: (String, String) -> JS -> Bool
isFn (moduleName, fnName) (JSAccessor x (JSVar y)) = x == fnName && y == moduleName
isFn (moduleName, fnName) (JSIndexer (JSStringLiteral x) (JSVar y)) = x == fnName && y == moduleName
isFn _ _ = False

isPreludeFn :: String -> JS -> Bool
isPreludeFn fnName = isFn (C.prelude, fnName)

semiringNumber :: (String, String)
semiringNumber = (C.prelude, C.semiringNumber)

semiringInt :: (String, String)
semiringInt = (C.prelude, C.semiringInt)

ringNumber :: (String, String)
ringNumber = (C.prelude, C.ringNumber)

ringInt :: (String, String)
ringInt = (C.prelude, C.ringInt)

moduloSemiringNumber :: (String, String)
moduloSemiringNumber = (C.prelude, C.moduloSemiringNumber)

moduloSemiringInt :: (String, String)
moduloSemiringInt = (C.prelude, C.moduloSemiringInt)

eqNumber :: (String, String)
eqNumber = (C.prelude, C.eqNumber)

eqInt :: (String, String)
eqInt = (C.prelude, C.eqInt)

eqString :: (String, String)
eqString = (C.prelude, C.eqNumber)

eqBoolean :: (String, String)
eqBoolean = (C.prelude, C.eqNumber)

ordNumber :: (String, String)
ordNumber = (C.prelude, C.ordNumber)

ordInt :: (String, String)
ordInt = (C.prelude, C.ordInt)

semigroupString :: (String, String)
semigroupString = (C.prelude, C.semigroupString)

boundedBoolean :: (String, String)
boundedBoolean = (C.prelude, C.boundedBoolean)

booleanAlgebraBoolean :: (String, String)
booleanAlgebraBoolean = (C.prelude, C.booleanAlgebraBoolean)

semigroupoidFn :: (String, String)
semigroupoidFn = (C.prelude, C.semigroupoidFn)
