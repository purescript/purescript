-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.CodeGen.JS.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Optimizer.Common
import qualified Language.PureScript.Constants as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: JS -> Bool
shouldInline (JSVar _ _) = True
shouldInline (JSNumericLiteral _ _) = True
shouldInline (JSStringLiteral _ _) = True
shouldInline (JSBooleanLiteral _ _) = True
shouldInline (JSAccessor _ _ val) = shouldInline val
shouldInline (JSIndexer _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: JS -> JS
etaConvert = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSBlock ss [JSReturn _ (JSApp _ (JSFunction _ Nothing idents block@(JSBlock _ body)) args)])
    | all shouldInline args &&
      not (any (`isRebound` block) (map (JSVar Nothing) idents)) &&
      not (any (`isRebound` block) args)
      = JSBlock ss (map (replaceIdents (zip idents args)) body)
  convert (JSFunction _ Nothing [] (JSBlock _ [JSReturn _ (JSApp _ fn [])])) = fn
  convert js = js

unThunk :: JS -> JS
unThunk = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSBlock ss []) = JSBlock ss []
  convert (JSBlock ss jss) =
    case last jss of
      JSReturn _ (JSApp _ (JSFunction _ Nothing [] (JSBlock _ body)) []) -> JSBlock ss $ init jss ++ body
      _ -> JSBlock ss jss
  convert js = js

evaluateIifes :: JS -> JS
evaluateIifes = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSApp _ (JSFunction _ Nothing [] (JSBlock _ [JSReturn _ ret])) []) = ret
  convert js = js

inlineVariables :: JS -> JS
inlineVariables = everywhereOnJS $ removeFromBlock go
  where
  go :: [JS] -> [JS]
  go [] = []
  go (JSVariableIntroduction _ var (Just js) : sts)
    | shouldInline js && not (any (isReassigned var) sts) && not (any (isRebound js) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var js) sts)
  go (s:sts) = s : go sts

inlineCommonValues :: JS -> JS
inlineCommonValues = everywhereOnJS convert
  where
  convert :: JS -> JS
  convert (JSApp ss fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isFn fnZero fn = JSNumericLiteral ss (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isFn fnOne fn = JSNumericLiteral ss (Left 1)
    | isDict boundedBoolean dict && isFn fnBottom fn = JSBooleanLiteral ss False
    | isDict boundedBoolean dict && isFn fnTop fn = JSBooleanLiteral ss True
  convert (JSApp ss (JSApp _ (JSApp _ fn [dict]) [x]) [y])
    | isDict semiringInt dict && isFn fnAdd fn = intOp ss Add x y
    | isDict semiringInt dict && isFn fnMultiply fn = intOp ss Multiply x y
    | isDict euclideanRingInt dict && isFn fnDivide fn = intOp ss Divide x y
    | isDict ringInt dict && isFn fnSubtract fn = intOp ss Subtract x y
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)
  fnAdd = (C.dataSemiring, C.add)
  fnDivide = (C.dataEuclideanRing, C.div)
  fnMultiply = (C.dataSemiring, C.mul)
  fnSubtract = (C.dataRing, C.sub)
  intOp ss op x y = JSBinary ss BitwiseOr (JSBinary ss op x y) (JSNumericLiteral ss (Left 0))

inlineCommonOperators :: JS -> JS
inlineCommonOperators = applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary euclideanRingNumber opDiv Divide
  , binary euclideanRingInt opMod Modulus

  , binary eqNumber opEq EqualTo
  , binary eqNumber opNotEq NotEqualTo
  , binary eqInt opEq EqualTo
  , binary eqInt opNotEq NotEqualTo
  , binary eqString opEq EqualTo
  , binary eqString opNotEq NotEqualTo
  , binary eqChar opEq EqualTo
  , binary eqChar opNotEq NotEqualTo
  , binary eqBoolean opEq EqualTo
  , binary eqBoolean opNotEq NotEqualTo

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqualTo
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqualTo
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqualTo
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqualTo
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqualTo
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqualTo
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqualTo

  , binary semigroupString opAppend Add

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary  heytingAlgebraBoolean opNot Not

  , binary' C.dataIntBits C.or BitwiseOr
  , binary' C.dataIntBits C.and BitwiseAnd
  , binary' C.dataIntBits C.xor BitwiseXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.dataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (isModFn (C.dataFunction, C.apply)) $ \f x -> JSApp Nothing f [x]
  , inlineNonClassFunction (isModFn (C.dataFunction, C.applyFlipped)) $ \x f -> JSApp Nothing f [x]
  , inlineNonClassFunction (isModFnWithDict (C.dataArray, C.unsafeIndex)) $ flip (JSIndexer Nothing)
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ]
  where
  binary :: (Text, Text) -> (Text, Text) -> BinaryOperator -> JS -> JS
  binary dict fns op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ (JSApp _ fn [dict']) [x]) [y]) | isDict dict dict' && isFn fns fn = JSBinary ss op x y
    convert other = other
  binary' :: Text -> Text -> BinaryOperator -> JS -> JS
  binary' moduleName opString op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ fn [x]) [y]) | isFn (moduleName, opString) fn = JSBinary ss op x y
    convert other = other
  unary :: (Text, Text) -> (Text, Text) -> UnaryOperator -> JS -> JS
  unary dicts fns op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ fn [dict']) [x]) | isDict dicts dict' && isFn fns fn = JSUnary ss op x
    convert other = other
  unary' :: Text -> Text -> UnaryOperator -> JS -> JS
  unary' moduleName fnName op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp ss fn [x]) | isFn (moduleName, fnName) fn = JSUnary ss op x
    convert other = other
  mkFn :: Int -> JS -> JS
  mkFn 0 = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp _ mkFnN [JSFunction s1 Nothing [_] (JSBlock s2 js)]) | isNFn C.mkFn 0 mkFnN =
      JSFunction s1 Nothing [] (JSBlock s2 js)
    convert other = other
  mkFn n = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert orig@(JSApp ss mkFnN [fn]) | isNFn C.mkFn n mkFnN =
      case collectArgs n [] fn of
        Just (args, js) -> JSFunction ss Nothing args (JSBlock ss js)
        Nothing -> orig
    convert other = other
    collectArgs :: Int -> [Text] -> JS -> Maybe ([Text], [JS])
    collectArgs 1 acc (JSFunction _ Nothing [oneArg] (JSBlock _ js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (JSFunction _ Nothing [oneArg] (JSBlock _ [JSReturn _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: Text -> Int -> JS -> Bool
  isNFn prefix n (JSVar _ name) = name == (prefix <> T.pack (show n))
  isNFn prefix n (JSAccessor _ name (JSVar _ dataFunctionUncurried)) | dataFunctionUncurried == C.dataFunctionUncurried = name == (prefix <> T.pack (show n))
  isNFn _ _ _ = False

  runFn :: Int -> JS -> JS
  runFn n = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [JS] -> JS -> Maybe JS
    go 0 acc (JSApp ss runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (JSApp ss fn acc)
    go m acc (JSApp _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (JS -> Bool) -> (JS -> JS -> JS) -> JS -> JS
  inlineNonClassFunction p f = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp _ (JSApp _ op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFn :: (Text, Text) -> JS -> Bool
  isModFn (m, op) (JSAccessor _ op' (JSVar _ m')) = m == m' && op == op'
  isModFn _ _ = False

  isModFnWithDict :: (Text, Text) -> JS -> Bool
  isModFnWithDict (m, op) (JSApp _ (JSAccessor _ op' (JSVar _ m')) [JSVar _ _]) = m == m' && op == op'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: (MonadSupply m) => JS -> m JS
inlineFnComposition = everywhereOnJSTopDownM convert
  where
  convert :: (MonadSupply m) => JS -> m JS
  convert (JSApp s1 (JSApp s2 (JSApp _ (JSApp _ fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ JSApp s1 x [JSApp s2 y [z]]
    | isFnComposeFlipped dict' fn = return $ JSApp s2 y [JSApp s1 x [z]]
  convert (JSApp ss (JSApp _ (JSApp _ fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ JSFunction ss Nothing [arg] (JSBlock ss [JSReturn Nothing $ JSApp Nothing x [JSApp Nothing y [JSVar Nothing arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ JSFunction ss Nothing [arg] (JSBlock ss [JSReturn Nothing $ JSApp Nothing y [JSApp Nothing x [JSVar Nothing arg]]])
  convert other = return other
  isFnCompose :: JS -> JS -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isFn fnCompose fn
  isFnComposeFlipped :: JS -> JS -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isFn fnComposeFlipped fn
  fnCompose :: (Text, Text)
  fnCompose = (C.controlSemigroupoid, C.compose)
  fnComposeFlipped :: (Text, Text)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

semiringNumber :: (Text, Text)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: (Text, Text)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: (Text, Text)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: (Text, Text)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: (Text, Text)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

euclideanRingInt :: (Text, Text)
euclideanRingInt = (C.dataEuclideanRing, C.euclideanRingInt)

eqNumber :: (Text, Text)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: (Text, Text)
eqInt = (C.dataEq, C.eqInt)

eqString :: (Text, Text)
eqString = (C.dataEq, C.eqString)

eqChar :: (Text, Text)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: (Text, Text)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: (Text, Text)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: (Text, Text)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: (Text, Text)
ordInt = (C.dataOrd, C.ordInt)

ordString :: (Text, Text)
ordString = (C.dataOrd, C.ordString)

ordChar :: (Text, Text)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: (Text, Text)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: (Text, Text)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: (Text, Text)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: (Text, Text)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: (Text, Text)
opAdd = (C.dataSemiring, C.add)

opMul :: (Text, Text)
opMul = (C.dataSemiring, C.mul)

opEq :: (Text, Text)
opEq = (C.dataEq, C.eq)

opNotEq :: (Text, Text)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: (Text, Text)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: (Text, Text)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: (Text, Text)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: (Text, Text)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: (Text, Text)
opAppend = (C.dataSemigroup, C.append)

opSub :: (Text, Text)
opSub = (C.dataRing, C.sub)

opNegate :: (Text, Text)
opNegate = (C.dataRing, C.negate)

opDiv :: (Text, Text)
opDiv = (C.dataEuclideanRing, C.div)

opMod :: (Text, Text)
opMod = (C.dataEuclideanRing, C.mod)

opConj :: (Text, Text)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: (Text, Text)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: (Text, Text)
opNot = (C.dataHeytingAlgebra, C.not)
