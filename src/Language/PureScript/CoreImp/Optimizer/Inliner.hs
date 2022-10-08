-- | This module performs basic inlining of known functions
module Language.PureScript.CoreImp.Optimizer.Inliner
  ( inlineVariables
  , inlineCommonValues
  , inlineCommonOperators
  , inlineFnComposition
  , inlineFnIdentity
  , inlineUnsafeCoerce
  , inlineUnsafePartial
  , etaConvert
  , unThunk
  , evaluateIifes
  ) where

import Prelude

import Control.Monad.Supply.Class (MonadSupply, freshName)

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (PSString)
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import Language.PureScript.AST (SourceSpan(..))
import qualified Language.PureScript.Constants.Prelude as C
import qualified Language.PureScript.Constants.Prim as C

-- TODO: Potential bug:
-- Shouldn't just inline this case: { var x = 0; x.toFixed(10); }
-- Needs to be: { 0..toFixed(10); }
-- Probably needs to be fixed in pretty-printer instead.
shouldInline :: AST -> Bool
shouldInline (Var _ _) = True
shouldInline (ModuleAccessor _ _ _) = True
shouldInline (NumericLiteral _ _) = True
shouldInline (StringLiteral _ _) = True
shouldInline (BooleanLiteral _ _) = True
shouldInline (Indexer _ index val) = shouldInline index && shouldInline val
shouldInline _ = False

etaConvert :: AST -> AST
etaConvert = everywhere convert
  where
  convert :: AST -> AST
  convert (Block ss [Return _ (App _ (Function _ Nothing idents block@(Block _ body)) args)])
    | all shouldInline args &&
      not (any ((`isRebound` block) . Var Nothing) idents) &&
      not (any (`isRebound` block) args)
      = Block ss (map (replaceIdents (zip idents args)) body)
  convert (Function _ Nothing [] (Block _ [Return _ (App _ fn [])])) = fn
  convert js = js

unThunk :: AST -> AST
unThunk = everywhere convert
  where
  convert :: AST -> AST
  convert (Block ss []) = Block ss []
  convert (Block ss jss) =
    case last jss of
      Return _ (App _ (Function _ Nothing [] (Block _ body)) []) -> Block ss $ init jss ++ body
      _ -> Block ss jss
  convert js = js

evaluateIifes :: AST -> AST
evaluateIifes = everywhere convert
  where
  convert :: AST -> AST
  convert (App _ (Function _ Nothing [] (Block _ [Return _ ret])) []) = ret
  convert (App _ (Function _ Nothing idents (Block _ [Return ss ret])) [])
    | not (any (`isReassigned` ret) idents) = replaceIdents (map (, Var ss C.undefined) idents) ret
  convert js = js

inlineVariables :: AST -> AST
inlineVariables = everywhere $ removeFromBlock go
  where
  go :: [AST] -> [AST]
  go [] = []
  go (VariableIntroduction _ var (Just (_, js)) : sts)
    | shouldInline js && not (any (isReassigned var) sts) && not (any (isRebound js) sts) && not (any (isUpdated var) sts) =
      go (map (replaceIdent var js) sts)
  go (s:sts) = s : go sts

inlineCommonValues :: (AST -> AST) -> AST -> AST
inlineCommonValues expander = everywhere convert
  where
  convert :: AST -> AST
  convert (expander -> App ss fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isDict fnZero fn = NumericLiteral ss (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isDict fnOne fn = NumericLiteral ss (Left 1)
    | isDict boundedBoolean dict && isDict fnBottom fn = BooleanLiteral ss False
    | isDict boundedBoolean dict && isDict fnTop fn = BooleanLiteral ss True
  convert (App ss (expander -> App _ fn [dict]) [x])
    | isDict ringInt dict && isDict fnNegate fn = Binary ss BitwiseOr (Unary ss Negate x) (NumericLiteral ss (Left 0))
  convert (App ss (App _ (expander -> App _ fn [dict]) [x]) [y])
    | isDict semiringInt dict && isDict fnAdd fn = intOp ss Add x y
    | isDict semiringInt dict && isDict fnMultiply fn = intOp ss Multiply x y
    | isDict ringInt dict && isDict fnSubtract fn = intOp ss Subtract x y
  convert other = other
  fnZero = (C.DataSemiring, C.zero)
  fnOne = (C.DataSemiring, C.one)
  fnBottom = (C.DataBounded, C.bottom)
  fnTop = (C.DataBounded, C.top)
  fnAdd = (C.DataSemiring, C.add)
  fnMultiply = (C.DataSemiring, C.mul)
  fnSubtract = (C.DataRing, C.sub)
  fnNegate = (C.DataRing, C.negate)
  intOp ss op x y = Binary ss BitwiseOr (Binary ss op x y) (NumericLiteral ss (Left 0))

inlineCommonOperators :: (AST -> AST) -> AST -> AST
inlineCommonOperators expander = everywhereTopDown $ applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary ringNumber opSub Subtract
  , unary  ringNumber opNegate Negate

  , binary euclideanRingNumber opDiv Divide

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

  , binary' C.DataIntBits C.or BitwiseOr
  , binary' C.DataIntBits C.and BitwiseAnd
  , binary' C.DataIntBits C.xor BitwiseXor
  , binary' C.DataIntBits C.shl ShiftLeft
  , binary' C.DataIntBits C.shr ShiftRight
  , binary' C.DataIntBits C.zshr ZeroFillShiftRight
  , unary'  C.DataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (isModFnWithDict (C.DataArray, C.unsafeIndex)) $ flip (Indexer Nothing)
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn C.ControlMonadEffUncurried C.mkEffFn i, runEffFn C.ControlMonadEffUncurried C.runEffFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn C.EffectUncurried C.mkEffectFn i, runEffFn C.EffectUncurried C.runEffectFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn C.ControlMonadSTUncurried C.mkSTFn i, runEffFn C.ControlMonadSTUncurried C.runSTFn i ] ]
  where
  binary :: (ModuleName, PSString) -> (ModuleName, PSString) -> BinaryOperator -> AST -> AST
  binary dict fns op = convert where
    convert :: AST -> AST
    convert (App ss (App _ (expander -> App _ fn [dict']) [x]) [y]) | isDict dict dict' && isDict fns fn = Binary ss op x y
    convert other = other
  binary' :: ModuleName -> PSString -> BinaryOperator -> AST -> AST
  binary' moduleName opString op = convert where
    convert :: AST -> AST
    convert (App ss (App _ fn [x]) [y]) | isDict (moduleName, opString) fn = Binary ss op x y
    convert other = other
  unary :: (ModuleName, PSString) -> (ModuleName, PSString) -> UnaryOperator -> AST -> AST
  unary dicts fns op = convert where
    convert :: AST -> AST
    convert (App ss (expander -> App _ fn [dict']) [x]) | isDict dicts dict' && isDict fns fn = Unary ss op x
    convert other = other
  unary' :: ModuleName -> PSString -> UnaryOperator -> AST -> AST
  unary' moduleName fnName op = convert where
    convert :: AST -> AST
    convert (App ss fn [x]) | isDict (moduleName, fnName) fn = Unary ss op x
    convert other = other

  mkFn :: Int -> AST -> AST
  mkFn = mkFn' C.DataFunctionUncurried C.mkFn $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 js])

  mkEffFn :: ModuleName -> Text -> Int -> AST -> AST
  mkEffFn modName fnName = mkFn' modName fnName $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 (App ss3 js [])])

  mkFn' :: ModuleName -> Text -> (Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan -> [Text] -> AST -> AST) -> Int -> AST -> AST
  mkFn' modName fnName res 0 = convert where
    convert :: AST -> AST
    convert (App _ mkFnN [Function s1 Nothing [_] (Block s2 [Return s3 js])]) | isNFn modName fnName 0 mkFnN =
      res s1 s2 s3 [] js
    convert other = other
  mkFn' modName fnName res n = convert where
    convert :: AST -> AST
    convert orig@(App ss mkFnN [fn]) | isNFn modName fnName n mkFnN =
      case collectArgs n [] fn of
        Just (args, [Return ss' ret]) -> res ss ss ss' args ret
        _ -> orig
    convert other = other
    collectArgs :: Int -> [Text] -> AST -> Maybe ([Text], [AST])
    collectArgs 1 acc (Function _ Nothing [oneArg] (Block _ js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (Function _ Nothing [oneArg] (Block _ [Return _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: ModuleName -> Text -> Int -> AST -> Bool
  isNFn expectMod prefix n (ModuleAccessor _ modName name) | modName == expectMod =
    name == fromString (T.unpack prefix <> show n)
  isNFn _ _ _ _ = False

  runFn :: Int -> AST -> AST
  runFn = runFn' C.DataFunctionUncurried C.runFn App

  runEffFn :: ModuleName -> Text -> Int -> AST -> AST
  runEffFn modName fnName = runFn' modName fnName $ \ss fn acc ->
    Function ss Nothing [] (Block ss [Return ss (App ss fn acc)])

  runFn' :: ModuleName -> Text -> (Maybe SourceSpan -> AST -> [AST] -> AST) -> Int -> AST -> AST
  runFn' modName runFnName res n = convert where
    convert :: AST -> AST
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [AST] -> AST -> Maybe AST
    go 0 acc (App ss runFnN [fn]) | isNFn modName runFnName n runFnN && length acc == n =
      Just $ res ss fn acc
    go m acc (App _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (AST -> Bool) -> (AST -> AST -> AST) -> AST -> AST
  inlineNonClassFunction p f = convert where
    convert :: AST -> AST
    convert (App _ (App _ op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFnWithDict :: (ModuleName, PSString) -> AST -> Bool
  isModFnWithDict (m, op) (App _ (ModuleAccessor _ m' op') [Var _ _]) =
    m == m' && op == op'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => (AST -> AST) -> AST -> m AST
inlineFnComposition expander = everywhereTopDownM convert
  where
  convert :: AST -> m AST
  convert (App s1 (App s2 (App _ (expander -> App _ fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ App s1 x [App s2 y [z]]
    | isFnComposeFlipped dict' fn = return $ App s2 y [App s1 x [z]]
  convert app@(App ss (App _ (expander -> App _ fn [dict']) _) _)
    | isFnCompose dict' fn || isFnComposeFlipped dict' fn = mkApps ss <$> goApps app <*> freshName
  convert other = return other

  mkApps :: Maybe SourceSpan -> [Either AST (Text, AST)] -> Text -> AST
  mkApps ss fns a = App ss (Function ss Nothing [] (Block ss $ vars <> [Return Nothing comp])) []
    where
    vars = uncurry (VariableIntroduction ss) . fmap (Just . (UnknownEffects, )) <$> rights fns
    comp = Function ss Nothing [a] (Block ss [Return Nothing apps])
    apps = foldr (\fn acc -> App ss (mkApp fn) [acc]) (Var ss a) fns

  mkApp :: Either AST (Text, AST) -> AST
  mkApp = either id $ \(name, arg) -> Var (getSourceSpan arg) name

  goApps :: AST -> m [Either AST (Text, AST)]
  goApps (App _ (App _ (expander -> App _ fn [dict']) [x]) [y])
    | isFnCompose dict' fn = mappend <$> goApps x <*> goApps y
    | isFnComposeFlipped dict' fn = mappend <$> goApps y <*> goApps x
  goApps app@App {} = pure . Right . (,app) <$> freshName
  goApps other = pure [Left other]

  isFnCompose :: AST -> AST -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isDict fnCompose fn

  isFnComposeFlipped :: AST -> AST -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isDict fnComposeFlipped fn

  fnCompose :: forall a. IsString a => (ModuleName, a)
  fnCompose = (C.ControlSemigroupoid, C.compose)

  fnComposeFlipped :: forall a. IsString a => (ModuleName, a)
  fnComposeFlipped = (C.ControlSemigroupoid, C.composeFlipped)

inlineFnIdentity :: (AST -> AST) -> AST -> AST
inlineFnIdentity expander = everywhereTopDown convert
  where
  convert :: AST -> AST
  convert (App _ (expander -> App _ fn [dict]) [x]) | isDict categoryFn dict && isDict fnIdentity fn = x
  convert other = other

  fnIdentity :: forall a. IsString a => (ModuleName, a)
  fnIdentity = (C.ControlCategory, C.identity)

inlineUnsafeCoerce :: AST -> AST
inlineUnsafeCoerce = everywhereTopDown convert where
  convert (App _ (ModuleAccessor _ C.UnsafeCoerce unsafeCoerceFn) [ comp ])
    | unsafeCoerceFn == C.unsafeCoerceFn
    = comp
  convert other = other

inlineUnsafePartial :: AST -> AST
inlineUnsafePartial = everywhereTopDown convert where
  convert (App ss (ModuleAccessor _ C.PartialUnsafe unsafePartial) [ comp ])
    | unsafePartial == C.unsafePartial
    -- Apply to undefined here, the application should be optimized away
    -- if it is safe to do so
    = App ss comp [ Var ss C.undefined ]
  convert other = other

semiringNumber :: forall a. IsString a => (ModuleName, a)
semiringNumber = (C.DataSemiring, C.semiringNumber)

semiringInt :: forall a. IsString a => (ModuleName, a)
semiringInt = (C.DataSemiring, C.semiringInt)

ringNumber :: forall a. IsString a => (ModuleName, a)
ringNumber = (C.DataRing, C.ringNumber)

ringInt :: forall a. IsString a => (ModuleName, a)
ringInt = (C.DataRing, C.ringInt)

euclideanRingNumber :: forall a. IsString a => (ModuleName, a)
euclideanRingNumber = (C.DataEuclideanRing, C.euclideanRingNumber)

eqNumber :: forall a. IsString a => (ModuleName, a)
eqNumber = (C.DataEq, C.eqNumber)

eqInt :: forall a. IsString a => (ModuleName, a)
eqInt = (C.DataEq, C.eqInt)

eqString :: forall a. IsString a => (ModuleName, a)
eqString = (C.DataEq, C.eqString)

eqChar :: forall a. IsString a => (ModuleName, a)
eqChar = (C.DataEq, C.eqChar)

eqBoolean :: forall a. IsString a => (ModuleName, a)
eqBoolean = (C.DataEq, C.eqBoolean)

ordBoolean :: forall a. IsString a => (ModuleName, a)
ordBoolean = (C.DataOrd, C.ordBoolean)

ordNumber :: forall a. IsString a => (ModuleName, a)
ordNumber = (C.DataOrd, C.ordNumber)

ordInt :: forall a. IsString a => (ModuleName, a)
ordInt = (C.DataOrd, C.ordInt)

ordString :: forall a. IsString a => (ModuleName, a)
ordString = (C.DataOrd, C.ordString)

ordChar :: forall a. IsString a => (ModuleName, a)
ordChar = (C.DataOrd, C.ordChar)

semigroupString :: forall a. IsString a => (ModuleName, a)
semigroupString = (C.DataSemigroup, C.semigroupString)

boundedBoolean :: forall a. IsString a => (ModuleName, a)
boundedBoolean = (C.DataBounded, C.boundedBoolean)

heytingAlgebraBoolean :: forall a. IsString a => (ModuleName, a)
heytingAlgebraBoolean = (C.DataHeytingAlgebra, C.heytingAlgebraBoolean)

semigroupoidFn :: forall a. IsString a => (ModuleName, a)
semigroupoidFn = (C.ControlSemigroupoid, C.semigroupoidFn)

categoryFn :: forall a. IsString a => (ModuleName, a)
categoryFn = (C.ControlCategory, C.categoryFn)

opAdd :: forall a. IsString a => (ModuleName, a)
opAdd = (C.DataSemiring, C.add)

opMul :: forall a. IsString a => (ModuleName, a)
opMul = (C.DataSemiring, C.mul)

opEq :: forall a. IsString a => (ModuleName, a)
opEq = (C.DataEq, C.eq)

opNotEq :: forall a. IsString a => (ModuleName, a)
opNotEq = (C.DataEq, C.notEq)

opLessThan :: forall a. IsString a => (ModuleName, a)
opLessThan = (C.DataOrd, C.lessThan)

opLessThanOrEq :: forall a. IsString a => (ModuleName, a)
opLessThanOrEq = (C.DataOrd, C.lessThanOrEq)

opGreaterThan :: forall a. IsString a => (ModuleName, a)
opGreaterThan = (C.DataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a. IsString a => (ModuleName, a)
opGreaterThanOrEq = (C.DataOrd, C.greaterThanOrEq)

opAppend :: forall a. IsString a => (ModuleName, a)
opAppend = (C.DataSemigroup, C.append)

opSub :: forall a. IsString a => (ModuleName, a)
opSub = (C.DataRing, C.sub)

opNegate :: forall a. IsString a => (ModuleName, a)
opNegate = (C.DataRing, C.negate)

opDiv :: forall a. IsString a => (ModuleName, a)
opDiv = (C.DataEuclideanRing, C.div)

opConj :: forall a. IsString a => (ModuleName, a)
opConj = (C.DataHeytingAlgebra, C.conj)

opDisj :: forall a. IsString a => (ModuleName, a)
opDisj = (C.DataHeytingAlgebra, C.disj)

opNot :: forall a. IsString a => (ModuleName, a)
opNot = (C.DataHeytingAlgebra, C.not)
