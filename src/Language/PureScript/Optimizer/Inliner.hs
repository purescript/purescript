-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Optimizer.Inliner
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

module Language.PureScript.Optimizer.Inliner (
  inlineVariables,
  inlineOperator,
  inlineCommonOperators,
  etaConvert,
  unThunk
) where

import Data.Generics

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.Common (identToJs)
import Language.PureScript.Optimizer.Common
import Language.PureScript.Names

shouldInline :: JS -> Bool
shouldInline (JSVar _) = True
shouldInline (JSNumericLiteral _) = True
shouldInline (JSStringLiteral _) = True
shouldInline (JSBooleanLiteral _) = True
shouldInline (JSAccessor _ val) = shouldInline val
shouldInline (JSIndexer index val) = shouldInline index && shouldInline val
shouldInline _ = False

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

inlineVariables :: JS -> JS
inlineVariables = everywhere (mkT $ removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction var (Just js) : sts)
    | shouldInline js && not (isReassigned var sts) && not (isRebound js sts) && not (isUpdated var sts) =
      go (replaceIdent var js sts)
  go (s:sts) = s : go sts

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
