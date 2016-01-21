-- |
-- This module provides basic inlining capabilities
--
module Language.PureScript.CodeGen.JS.Optimizer.Inliner
  ( inlineVariables
  , inlineValues
  , inlineOperator
  , inlineCommonOperators
  , inlineFnComposition
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude ()
import Prelude.Compat

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
  convert (JSApp fn [dict])
    | isDict' (semiringNumber ++ semiringInt) dict && isFn' fnZero fn = JSNumericLiteral (Left 0)
    | isDict' (semiringNumber ++ semiringInt) dict && isFn' fnOne fn = JSNumericLiteral (Left 1)
    | isDict' boundedBoolean dict && isFn' fnBottom fn = JSBooleanLiteral False
    | isDict' boundedBoolean dict && isFn' fnTop fn = JSBooleanLiteral True
  convert (JSApp (JSApp (JSApp fn [dict]) [x]) [y])
    | isDict' semiringInt dict && isFn' fnAdd fn = intOp Add x y
    | isDict' semiringInt dict && isFn' fnMultiply fn = intOp Multiply x y
    | isDict' moduloSemiringInt dict && isFn' fnDivide fn = intOp Divide x y
    | isDict' ringInt dict && isFn' fnSubtract fn = intOp Subtract x y
  convert other = other
  fnZero = [(C.prelude, C.zero), (C.dataSemiring, C.zero)]
  fnOne = [(C.prelude, C.one), (C.dataSemiring, C.one)]
  fnBottom = [(C.prelude, C.bottom), (C.dataBounded, C.bottom)]
  fnTop = [(C.prelude, C.top), (C.dataBounded, C.top)]
  fnAdd = [(C.prelude, (C.+)), (C.prelude, (C.add)), (C.dataSemiring, (C.+)), (C.dataSemiring, (C.add))]
  fnDivide = [(C.prelude, (C./)), (C.prelude, (C.div)), (C.dataModuloSemiring, C.div)]
  fnMultiply = [(C.prelude, (C.*)), (C.prelude, (C.mul)), (C.dataSemiring, (C.*)), (C.dataSemiring, (C.mul))]
  fnSubtract = [(C.prelude, (C.-)), (C.prelude, C.sub), (C.dataRing, C.sub)]
  intOp op x y = JSBinary BitwiseOr (JSBinary op x y) (JSNumericLiteral (Left 0))

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
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate
  , binary ringInt opSub Subtract
  , unary  ringInt opNegate Negate

  , binary moduloSemiringNumber opDiv Divide
  , binary moduloSemiringInt opMod Modulus

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

  , binary booleanAlgebraBoolean opConj And
  , binary booleanAlgebraBoolean opDisj Or
  , unary  booleanAlgebraBoolean opNot Not

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
  binary :: [(String, String)] -> [(String, String)] -> BinaryOperator -> JS -> JS
  binary dict fns op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) | isDict' dict dict' && isFn' fns fn = JSBinary op x y
    convert other = other
  binary' :: String -> String -> BinaryOperator -> JS -> JS
  binary' moduleName opString op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [x]) [y]) | isFn (moduleName, opString) fn = JSBinary op x y
    convert other = other
  unary :: [(String, String)] -> [(String, String)] -> UnaryOperator -> JS -> JS
  unary dicts fns op = everywhereOnJS convert
    where
    convert :: JS -> JS
    convert (JSApp (JSApp fn [dict']) [x]) | isDict' dicts dict' && isFn' fns fn = JSUnary op x
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
  convert (JSApp (JSApp (JSApp (JSApp fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ JSApp x [JSApp y [z]]
    | isFnComposeFlipped dict' fn = return $ JSApp y [JSApp x [z]]
  convert (JSApp (JSApp (JSApp fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ JSFunction Nothing [arg] (JSBlock [JSReturn $ JSApp x [JSApp y [JSVar arg]]])
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ JSFunction Nothing [arg] (JSBlock [JSReturn $ JSApp y [JSApp x [JSVar arg]]])
  convert other = return other
  isFnCompose :: JS -> JS -> Bool
  isFnCompose dict' fn = isDict' semigroupoidFn dict' && isFn' fnCompose fn
  isFnComposeFlipped :: JS -> JS -> Bool
  isFnComposeFlipped dict' fn = isDict' semigroupoidFn dict' && isFn' fnComposeFlipped fn
  fnCompose :: [(String, String)]
  fnCompose = [(C.prelude, C.compose), (C.prelude, (C.<<<)), (C.controlSemigroupoid, C.compose)]
  fnComposeFlipped :: [(String, String)]
  fnComposeFlipped = [(C.prelude, (C.>>>)), (C.controlSemigroupoid, C.composeFlipped)]

semiringNumber :: [(String, String)]
semiringNumber = [(C.prelude, C.semiringNumber), (C.dataSemiring, C.semiringNumber)]

semiringInt :: [(String, String)]
semiringInt = [(C.prelude, C.semiringInt), (C.dataSemiring, C.semiringInt)]

ringNumber :: [(String, String)]
ringNumber = [(C.prelude, C.ringNumber), (C.dataRing, C.ringNumber)]

ringInt :: [(String, String)]
ringInt = [(C.prelude, C.ringInt), (C.dataRing, C.ringInt)]

moduloSemiringNumber :: [(String, String)]
moduloSemiringNumber = [(C.prelude, C.moduloSemiringNumber), (C.dataModuloSemiring, C.moduloSemiringNumber)]

moduloSemiringInt :: [(String, String)]
moduloSemiringInt = [(C.prelude, C.moduloSemiringInt), (C.dataModuloSemiring, C.moduloSemiringInt)]

eqNumber :: [(String, String)]
eqNumber = [(C.prelude, C.eqNumber), (C.dataEq, C.eqNumber)]

eqInt :: [(String, String)]
eqInt = [(C.prelude, C.eqInt), (C.dataEq, C.eqInt)]

eqString :: [(String, String)]
eqString = [(C.prelude, C.eqString), (C.dataEq, C.eqString)]

eqChar :: [(String, String)]
eqChar = [(C.prelude, C.eqChar), (C.dataEq, C.eqChar)]

eqBoolean :: [(String, String)]
eqBoolean = [(C.prelude, C.eqBoolean), (C.dataEq, C.eqBoolean)]

ordBoolean :: [(String, String)]
ordBoolean = [(C.prelude, C.ordBoolean), (C.dataOrd, C.ordBoolean)]

ordNumber :: [(String, String)]
ordNumber = [(C.prelude, C.ordNumber), (C.dataOrd, C.ordNumber)]

ordInt :: [(String, String)]
ordInt = [(C.prelude, C.ordInt), (C.dataOrd, C.ordInt)]

ordString :: [(String, String)]
ordString = [(C.prelude, C.ordString), (C.dataOrd, C.ordString)]

ordChar :: [(String, String)]
ordChar = [(C.prelude, C.ordChar), (C.dataOrd, C.ordChar)]

semigroupString :: [(String, String)]
semigroupString = [(C.prelude, C.semigroupString), (C.dataSemigroup, C.semigroupString)]

boundedBoolean :: [(String, String)]
boundedBoolean = [(C.prelude, C.boundedBoolean), (C.dataBounded, C.boundedBoolean)]

booleanAlgebraBoolean :: [(String, String)]
booleanAlgebraBoolean = [(C.prelude, C.booleanAlgebraBoolean), (C.dataBooleanAlgebra, C.booleanAlgebraBoolean)]

semigroupoidFn :: [(String, String)]
semigroupoidFn = [(C.prelude, C.semigroupoidFn), (C.controlSemigroupoid, C.semigroupoidFn)]

opAdd :: [(String, String)]
opAdd = [(C.prelude, (C.+)), (C.prelude, C.add), (C.dataSemiring, C.add)]

opMul :: [(String, String)]
opMul = [(C.prelude, (C.*)), (C.prelude, C.mul), (C.dataSemiring, C.mul)]

opEq :: [(String, String)]
opEq = [(C.prelude, (C.==)), (C.prelude, C.eq), (C.dataEq, C.eq)]

opNotEq :: [(String, String)]
opNotEq = [(C.prelude, (C./=)), (C.dataEq, C.notEq)]

opLessThan :: [(String, String)]
opLessThan = [(C.prelude, (C.<)), (C.dataOrd, C.lessThan)]

opLessThanOrEq :: [(String, String)]
opLessThanOrEq = [(C.prelude, (C.<=)), (C.dataOrd, C.lessThanOrEq)]

opGreaterThan :: [(String, String)]
opGreaterThan = [(C.prelude, (C.>)), (C.dataOrd, C.greaterThan)]

opGreaterThanOrEq :: [(String, String)]
opGreaterThanOrEq = [(C.prelude, (C.>=)), (C.dataOrd, C.greaterThanOrEq)]

opAppend :: [(String, String)]
opAppend = [(C.prelude, (C.<>)), (C.prelude, (C.++)), (C.prelude, C.append), (C.dataSemigroup, C.append)]

opSub :: [(String, String)]
opSub = [(C.prelude, (C.-)), (C.prelude, C.sub), (C.dataRing, C.sub)]

opNegate :: [(String, String)]
opNegate = [(C.prelude, C.negate), (C.dataRing, C.negate)]

opDiv :: [(String, String)]
opDiv = [(C.prelude, (C./)), (C.prelude, C.div), (C.dataModuloSemiring, C.div)]

opMod :: [(String, String)]
opMod = [(C.prelude, C.mod), (C.dataModuloSemiring, C.mod)]

opConj :: [(String, String)]
opConj = [(C.prelude, (C.&&)), (C.prelude, C.conj), (C.dataBooleanAlgebra, C.conj)]

opDisj :: [(String, String)]
opDisj = [(C.prelude, (C.||)), (C.prelude, C.disj), (C.dataBooleanAlgebra, C.disj)]

opNot :: [(String, String)]
opNot = [(C.prelude, C.not), (C.dataBooleanAlgebra, C.not)]
