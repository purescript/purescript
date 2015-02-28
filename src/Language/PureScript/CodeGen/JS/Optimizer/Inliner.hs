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

module Language.PureScript.CodeGen.JS.Optimizer.Inliner (
  inlineVariables,
  inlineValues,
  inlineOperator,
  inlineCommonOperators,
  etaConvert,
  unThunk,
  evaluateIifes
) where

import Data.Maybe (fromMaybe)

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.CodeGen.JS.Optimizer.Common
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

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
  convert (JSApp fn [dict]) | isPreludeDict C.semiringNumber dict && isPreludeFn C.zero fn = JSNumericLiteral (Left 0)
  convert (JSApp fn [dict]) | isPreludeDict C.semiringNumber dict && isPreludeFn C.one fn = JSNumericLiteral (Left 1)
  convert (JSApp (JSApp fn [x]) [y]) | isPreludeFn (C.%) fn = JSBinary Modulus x y
  convert other = other

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
  [ binary C.semiringNumber (C.+) Add
  , binary C.semiringNumber (C.*) Multiply
  , binary C.ringNumber (C.-) Subtract
  , unary  C.ringNumber C.negate JSNegate
  , binary C.moduloSemiringNumber (C./) Divide

  , binary C.ordNumber (C.<) LessThan
  , binary C.ordNumber (C.>) GreaterThan
  , binary C.ordNumber (C.<=) LessThanOrEqual
  , binary C.ordNumber (C.>=) GreaterThanOrEqual

  , binary C.eqNumber (C.==) Equal
  , binary C.eqNumber (C./=) NotEqual
  , binary C.eqString (C.==) Equal
  , binary C.eqString (C./=) NotEqual
  , binary C.eqBoolean (C.==) Equal
  , binary C.eqBoolean (C./=) NotEqual

  , binary C.semigroupString (C.<>) Add
  , binary C.semigroupString (C.++) Add

  , binaryFunction C.bitsNumber C.shl ShiftLeft
  , binaryFunction C.bitsNumber C.shr ShiftRight
  , binaryFunction C.bitsNumber C.zshr ZeroFillShiftRight
  , binary         C.bitsNumber (C..&.) BitwiseAnd
  , binary         C.bitsNumber (C..|.) BitwiseOr
  , binary         C.bitsNumber (C..^.) BitwiseXor
  , unary          C.bitsNumber C.complement JSBitwiseNot

  , binary C.boolLikeBoolean (C.&&) And
  , binary C.boolLikeBoolean (C.||) Or
  , unary  C.boolLikeBoolean C.not JSNot
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: String -> String -> BinaryOp -> JS -> JS
  binary dictName opString op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isPreludeDict dictName dict && isPreludeFn opString fn = JSBinary op x y
    convert other = other
  binaryFunction :: String -> String -> BinaryOp -> JS -> JS
  binaryFunction dictName fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y]) | isPreludeFn fnName fn && isPreludeDict dictName dict = JSBinary op x y
    convert other = other
  unary :: String -> String -> JSUnaryOp -> JS -> JS
  unary dictName fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [dict]) [x]) | isPreludeFn fnName fn && isPreludeDict dictName dict = JSUnary op x
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

isPreludeDict :: String -> JS -> Bool
isPreludeDict dictName (JSAccessor prop (JSVar prelude)) = prelude == C.prelude && prop == dictName
isPreludeDict _ _ = False

isPreludeFn :: String -> JS -> Bool
isPreludeFn fnName (JSAccessor fnName' (JSVar prelude)) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (JSIndexer (JSStringLiteral fnName') (JSVar prelude)) = prelude == C.prelude && fnName' == fnName
isPreludeFn fnName (JSAccessor longForm (JSAccessor prelude (JSVar _))) = prelude == C.prelude && longForm == identToJs (Op fnName)
isPreludeFn _ _ = False
