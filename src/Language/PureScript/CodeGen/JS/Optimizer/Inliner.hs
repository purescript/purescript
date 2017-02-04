-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.CodeGen.JS.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , inlineUnsafePartial
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.PSString (PSString)
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
  convert (JSApp _ (JSFunction _ Nothing idents (JSBlock _ [JSReturn ss ret])) [])
    | not (any (`isReassigned` ret) idents) = replaceIdents (map (, JSVar ss C.undefined) idents) ret
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
    | isDict' [semiringNumber, semiringInt] dict && isDict fnZero fn = JSNumericLiteral ss (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isDict fnOne fn = JSNumericLiteral ss (Left 1)
    | isDict boundedBoolean dict && isDict fnBottom fn = JSBooleanLiteral ss False
    | isDict boundedBoolean dict && isDict fnTop fn = JSBooleanLiteral ss True
  convert (JSApp ss (JSApp _ fn [dict]) [x])
    | isDict ringInt dict && isDict fnNegate fn = JSBinary ss BitwiseOr (JSUnary ss Negate x) (JSNumericLiteral ss (Left 0))
  convert (JSApp ss (JSApp _ (JSApp _ fn [dict]) [x]) [y])
    | isDict semiringInt dict && isDict fnAdd fn = intOp ss Add x y
    | isDict semiringInt dict && isDict fnMultiply fn = intOp ss Multiply x y
    | isDict euclideanRingInt dict && isDict fnDivide fn = intOp ss Divide x y
    | isDict ringInt dict && isDict fnSubtract fn = intOp ss Subtract x y
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)
  fnAdd = (C.dataSemiring, C.add)
  fnDivide = (C.dataEuclideanRing, C.div)
  fnMultiply = (C.dataSemiring, C.mul)
  fnSubtract = (C.dataRing, C.sub)
  fnNegate = (C.dataRing, C.negate)
  intOp ss op x y = JSBinary ss BitwiseOr (JSBinary ss op x y) (JSNumericLiteral ss (Left 0))

inlineCommonOperators :: JS -> JS
inlineCommonOperators = everywhereOnJSTopDown $ applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate

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
  binary :: (Text, PSString) -> (Text, PSString) -> BinaryOperator -> JS -> JS
  binary dict fns op = convert where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ (JSApp _ fn [dict']) [x]) [y]) | isDict dict dict' && isDict fns fn = JSBinary ss op x y
    convert other = other
  binary' :: Text -> PSString -> BinaryOperator -> JS -> JS
  binary' moduleName opString op = convert where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ fn [x]) [y]) | isDict (moduleName, opString) fn = JSBinary ss op x y
    convert other = other
  unary :: (Text, PSString) -> (Text, PSString) -> UnaryOperator -> JS -> JS
  unary dicts fns op = convert where
    convert :: JS -> JS
    convert (JSApp ss (JSApp _ fn [dict']) [x]) | isDict dicts dict' && isDict fns fn = JSUnary ss op x
    convert other = other
  unary' :: Text -> PSString -> UnaryOperator -> JS -> JS
  unary' moduleName fnName op = convert where
    convert :: JS -> JS
    convert (JSApp ss fn [x]) | isDict (moduleName, fnName) fn = JSUnary ss op x
    convert other = other
  mkFn :: Int -> JS -> JS
  mkFn 0 = convert where
    convert :: JS -> JS
    convert (JSApp _ mkFnN [JSFunction s1 Nothing [_] (JSBlock s2 js)]) | isNFn C.mkFn 0 mkFnN =
      JSFunction s1 Nothing [] (JSBlock s2 js)
    convert other = other
  mkFn n = convert where
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
  isNFn prefix n (JSIndexer _ (JSStringLiteral _ name) (JSVar _ dataFunctionUncurried)) | dataFunctionUncurried == C.dataFunctionUncurried =
    name == fromString (T.unpack prefix <> show n)
  isNFn _ _ _ = False

  runFn :: Int -> JS -> JS
  runFn n = convert where
    convert :: JS -> JS
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [JS] -> JS -> Maybe JS
    go 0 acc (JSApp ss runFnN [fn]) | isNFn C.runFn n runFnN && length acc == n = Just (JSApp ss fn acc)
    go m acc (JSApp _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (JS -> Bool) -> (JS -> JS -> JS) -> JS -> JS
  inlineNonClassFunction p f = convert where
    convert :: JS -> JS
    convert (JSApp _ (JSApp _ op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFn :: (Text, PSString) -> JS -> Bool
  isModFn (m, op) (JSIndexer _ (JSStringLiteral _ op') (JSVar _ m')) =
    m == m' && op == op'
  isModFn _ _ = False

  isModFnWithDict :: (Text, PSString) -> JS -> Bool
  isModFnWithDict (m, op) (JSApp _ (JSIndexer _ (JSStringLiteral _ op') (JSVar _ m')) [JSVar _ _]) =
    m == m' && op == op'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => JS -> m JS
inlineFnComposition = everywhereOnJSTopDownM convert where
  convert :: JS -> m JS
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
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isDict fnCompose fn
  isFnComposeFlipped :: JS -> JS -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isDict fnComposeFlipped fn
  fnCompose :: forall a b. (IsString a, IsString b) => (a, b)
  fnCompose = (C.controlSemigroupoid, C.compose)
  fnComposeFlipped :: forall a b. (IsString a, IsString b) => (a, b)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

inlineUnsafePartial :: JS -> JS
inlineUnsafePartial = everywhereOnJSTopDown convert where
  convert (JSApp ss (JSIndexer _ (JSStringLiteral _ unsafePartial) (JSVar _ partialUnsafe)) [ comp ])
    | unsafePartial == C.unsafePartial && partialUnsafe == C.partialUnsafe
    -- Apply to undefined here, the application should be optimized away
    -- if it is safe to do so
    = JSApp ss comp [ JSVar ss C.undefined ]
  convert other = other

semiringNumber :: forall a b. (IsString a, IsString b) => (a, b)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b) => (a, b)
semiringInt = (C.dataSemiring, C.semiringInt)

ringNumber :: forall a b. (IsString a, IsString b) => (a, b)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: forall a b. (IsString a, IsString b) => (a, b)
ringInt = (C.dataRing, C.ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

euclideanRingInt :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingInt = (C.dataEuclideanRing, C.euclideanRingInt)

eqNumber :: forall a b. (IsString a, IsString b) => (a, b)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: forall a b. (IsString a, IsString b) => (a, b)
eqInt = (C.dataEq, C.eqInt)

eqString :: forall a b. (IsString a, IsString b) => (a, b)
eqString = (C.dataEq, C.eqString)

eqChar :: forall a b. (IsString a, IsString b) => (a, b)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: forall a b. (IsString a, IsString b) => (a, b)
eqBoolean = (C.dataEq, C.eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b) => (a, b)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b) => (a, b)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: forall a b. (IsString a, IsString b) => (a, b)
ordInt = (C.dataOrd, C.ordInt)

ordString :: forall a b. (IsString a, IsString b) => (a, b)
ordString = (C.dataOrd, C.ordString)

ordChar :: forall a b. (IsString a, IsString b) => (a, b)
ordChar = (C.dataOrd, C.ordChar)

semigroupString :: forall a b. (IsString a, IsString b) => (a, b)
semigroupString = (C.dataSemigroup, C.semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b) => (a, b)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b) => (a, b)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: forall a b. (IsString a, IsString b) => (a, b)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b) => (a, b)
opAdd = (C.dataSemiring, C.add)

opMul :: forall a b. (IsString a, IsString b) => (a, b)
opMul = (C.dataSemiring, C.mul)

opEq :: forall a b. (IsString a, IsString b) => (a, b)
opEq = (C.dataEq, C.eq)

opNotEq :: forall a b. (IsString a, IsString b) => (a, b)
opNotEq = (C.dataEq, C.notEq)

opLessThan :: forall a b. (IsString a, IsString b) => (a, b)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

opAppend :: forall a b. (IsString a, IsString b) => (a, b)
opAppend = (C.dataSemigroup, C.append)

opSub :: forall a b. (IsString a, IsString b) => (a, b)
opSub = (C.dataRing, C.sub)

opNegate :: forall a b. (IsString a, IsString b) => (a, b)
opNegate = (C.dataRing, C.negate)

opDiv :: forall a b. (IsString a, IsString b) => (a, b)
opDiv = (C.dataEuclideanRing, C.div)

opMod :: forall a b. (IsString a, IsString b) => (a, b)
opMod = (C.dataEuclideanRing, C.mod)

opConj :: forall a b. (IsString a, IsString b) => (a, b)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: forall a b. (IsString a, IsString b) => (a, b)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: forall a b. (IsString a, IsString b) => (a, b)
opNot = (C.dataHeytingAlgebra, C.not)
