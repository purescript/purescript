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
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.CoreImp.AST ( everywhere, everywhereTopDown, everywhereTopDownM, getSourceSpan, AST(ModuleAccessor, StringLiteral, BooleanLiteral, NumericLiteral, Binary, Unary, Indexer, VariableIntroduction, Function, Block, Return, Var, App), BinaryOperator(ZeroFillShiftRight, Multiply, Subtract, Divide, EqualTo, NotEqualTo, LessThan, LessThanOrEqualTo, GreaterThan, GreaterThanOrEqualTo, Add, And, Or, BitwiseOr, BitwiseAnd, BitwiseXor, ShiftLeft, ShiftRight), InitializerEffects(UnknownEffects), UnaryOperator(BitwiseNot, Negate, Not) )
import Language.PureScript.CoreImp.Optimizer.Common ( pattern Ref, applyAll, isReassigned, isRebound, isUpdated, removeFromBlock, replaceIdent, replaceIdents )
import Language.PureScript.AST.SourcePos ( SourceSpan(..) )
import Language.PureScript.Constants.Libs qualified as CLibs
import Language.PureScript.Constants.Prim qualified as CPrim

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
    | not (any (`isReassigned` ret) idents) = replaceIdents (map (, Var ss CPrim.S_undefined) idents) ret
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
  convert (expander -> App ss (Ref fn) [Ref dict])
    | dict `elem` [CLibs.P_semiringNumber, CLibs.P_semiringInt], CLibs.P_zero <- fn = NumericLiteral ss (Left 0)
    | dict `elem` [CLibs.P_semiringNumber, CLibs.P_semiringInt], CLibs.P_one <- fn = NumericLiteral ss (Left 1)
    | CLibs.P_boundedBoolean <- dict, CLibs.P_bottom <- fn = BooleanLiteral ss False
    | CLibs.P_boundedBoolean <- dict, CLibs.P_top <- fn = BooleanLiteral ss True
  convert (App ss (expander -> App _ (Ref CLibs.P_negate) [Ref CLibs.P_ringInt]) [x])
    = Binary ss BitwiseOr (Unary ss Negate x) (NumericLiteral ss (Left 0))
  convert (App ss (App _ (expander -> App _ (Ref fn) [Ref dict]) [x]) [y])
    | CLibs.P_semiringInt <- dict, CLibs.P_add <- fn = intOp ss Add x y
    | CLibs.P_semiringInt <- dict, CLibs.P_mul <- fn = intOp ss Multiply x y
    | CLibs.P_ringInt <- dict, CLibs.P_sub <- fn = intOp ss Subtract x y
  convert other = other
  intOp ss op x y = Binary ss BitwiseOr (Binary ss op x y) (NumericLiteral ss (Left 0))

inlineCommonOperators :: (AST -> AST) -> AST -> AST
inlineCommonOperators expander = everywhereTopDown $ applyAll $
  [ binary CLibs.P_semiringNumber CLibs.P_add Add
  , binary CLibs.P_semiringNumber CLibs.P_mul Multiply

  , binary CLibs.P_ringNumber CLibs.P_sub Subtract
  , unary  CLibs.P_ringNumber CLibs.P_negate Negate

  , binary CLibs.P_euclideanRingNumber CLibs.P_div Divide

  , binary CLibs.P_eqNumber CLibs.P_eq EqualTo
  , binary CLibs.P_eqNumber CLibs.P_notEq NotEqualTo
  , binary CLibs.P_eqInt CLibs.P_eq EqualTo
  , binary CLibs.P_eqInt CLibs.P_notEq NotEqualTo
  , binary CLibs.P_eqString CLibs.P_eq EqualTo
  , binary CLibs.P_eqString CLibs.P_notEq NotEqualTo
  , binary CLibs.P_eqChar CLibs.P_eq EqualTo
  , binary CLibs.P_eqChar CLibs.P_notEq NotEqualTo
  , binary CLibs.P_eqBoolean CLibs.P_eq EqualTo
  , binary CLibs.P_eqBoolean CLibs.P_notEq NotEqualTo

  , binary CLibs.P_ordBoolean CLibs.P_lessThan LessThan
  , binary CLibs.P_ordBoolean CLibs.P_lessThanOrEq LessThanOrEqualTo
  , binary CLibs.P_ordBoolean CLibs.P_greaterThan GreaterThan
  , binary CLibs.P_ordBoolean CLibs.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary CLibs.P_ordChar CLibs.P_lessThan LessThan
  , binary CLibs.P_ordChar CLibs.P_lessThanOrEq LessThanOrEqualTo
  , binary CLibs.P_ordChar CLibs.P_greaterThan GreaterThan
  , binary CLibs.P_ordChar CLibs.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary CLibs.P_ordInt CLibs.P_lessThan LessThan
  , binary CLibs.P_ordInt CLibs.P_lessThanOrEq LessThanOrEqualTo
  , binary CLibs.P_ordInt CLibs.P_greaterThan GreaterThan
  , binary CLibs.P_ordInt CLibs.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary CLibs.P_ordNumber CLibs.P_lessThan LessThan
  , binary CLibs.P_ordNumber CLibs.P_lessThanOrEq LessThanOrEqualTo
  , binary CLibs.P_ordNumber CLibs.P_greaterThan GreaterThan
  , binary CLibs.P_ordNumber CLibs.P_greaterThanOrEq GreaterThanOrEqualTo
  , binary CLibs.P_ordString CLibs.P_lessThan LessThan
  , binary CLibs.P_ordString CLibs.P_lessThanOrEq LessThanOrEqualTo
  , binary CLibs.P_ordString CLibs.P_greaterThan GreaterThan
  , binary CLibs.P_ordString CLibs.P_greaterThanOrEq GreaterThanOrEqualTo

  , binary CLibs.P_semigroupString CLibs.P_append Add

  , binary CLibs.P_heytingAlgebraBoolean CLibs.P_conj And
  , binary CLibs.P_heytingAlgebraBoolean CLibs.P_disj Or
  , unary  CLibs.P_heytingAlgebraBoolean CLibs.P_not Not

  , binary' CLibs.P_or BitwiseOr
  , binary' CLibs.P_and BitwiseAnd
  , binary' CLibs.P_xor BitwiseXor
  , binary' CLibs.P_shl ShiftLeft
  , binary' CLibs.P_shr ShiftRight
  , binary' CLibs.P_zshr ZeroFillShiftRight
  , unary'  CLibs.P_complement BitwiseNot

  , inlineNonClassFunction (isModFnWithDict CLibs.P_unsafeIndex) $ flip (Indexer Nothing)
  ] ++
  [ fn | i <- [0..10], fn <- [ mkFn i, runFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn CLibs.P_mkEffFn i, runEffFn CLibs.P_runEffFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn CLibs.P_mkEffectFn i, runEffFn CLibs.P_runEffectFn i ] ] ++
  [ fn | i <- [0..10], fn <- [ mkEffFn CLibs.P_mkSTFn i, runEffFn CLibs.P_runSTFn i ] ]
  where
  binary :: (ModuleName, PSString) -> (ModuleName, PSString) -> BinaryOperator -> AST -> AST
  binary dict fn op = convert where
    convert :: AST -> AST
    convert (App ss (App _ (expander -> App _ (Ref fn') [Ref dict']) [x]) [y]) | dict == dict', fn == fn' = Binary ss op x y
    convert other = other
  binary' :: (ModuleName, PSString) -> BinaryOperator -> AST -> AST
  binary' fn op = convert where
    convert :: AST -> AST
    convert (App ss (App _ (Ref fn') [x]) [y]) | fn == fn' = Binary ss op x y
    convert other = other
  unary :: (ModuleName, PSString) -> (ModuleName, PSString) -> UnaryOperator -> AST -> AST
  unary dict fn op = convert where
    convert :: AST -> AST
    convert (App ss (expander -> App _ (Ref fn') [Ref dict']) [x]) | dict == dict', fn == fn' = Unary ss op x
    convert other = other
  unary' :: (ModuleName, PSString) -> UnaryOperator -> AST -> AST
  unary' fn op = convert where
    convert :: AST -> AST
    convert (App ss (Ref fn') [x]) | fn == fn' = Unary ss op x
    convert other = other

  mkFn :: Int -> AST -> AST
  mkFn = mkFn' CLibs.P_mkFn $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 js])

  mkEffFn :: (ModuleName, PSString) -> Int -> AST -> AST
  mkEffFn mkFn_ = mkFn' mkFn_ $ \ss1 ss2 ss3 args js ->
    Function ss1 Nothing args (Block ss2 [Return ss3 (App ss3 js [])])

  mkFn' :: (ModuleName, PSString) -> (Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan -> [Text] -> AST -> AST) -> Int -> AST -> AST
  mkFn' mkFn_ res 0 = convert where
    convert :: AST -> AST
    convert (App _ (Ref mkFnN) [Function s1 Nothing [_] (Block s2 [Return s3 js])]) | isNFn mkFn_ 0 mkFnN =
      res s1 s2 s3 [] js
    convert other = other
  mkFn' mkFn_ res n = convert where
    convert :: AST -> AST
    convert orig@(App ss (Ref mkFnN) [fn]) | isNFn mkFn_ n mkFnN =
      case collectArgs n [] fn of
        Just (args, [Return ss' ret]) -> res ss ss ss' args ret
        _ -> orig
    convert other = other
    collectArgs :: Int -> [Text] -> AST -> Maybe ([Text], [AST])
    collectArgs 1 acc (Function _ Nothing [oneArg] (Block _ js)) | length acc == n - 1 = Just (reverse (oneArg : acc), js)
    collectArgs m acc (Function _ Nothing [oneArg] (Block _ [Return _ ret])) = collectArgs (m - 1) (oneArg : acc) ret
    collectArgs _ _   _ = Nothing

  isNFn :: (ModuleName, PSString) -> Int -> (ModuleName, PSString) -> Bool
  isNFn prefix n fn = fmap (<> mkString (T.pack $ show n)) prefix == fn

  runFn :: Int -> AST -> AST
  runFn = runFn' CLibs.P_runFn App

  runEffFn :: (ModuleName, PSString) -> Int -> AST -> AST
  runEffFn runFn_ = runFn' runFn_ $ \ss fn acc ->
    Function ss Nothing [] (Block ss [Return ss (App ss fn acc)])

  runFn' :: (ModuleName, PSString) -> (Maybe SourceSpan -> AST -> [AST] -> AST) -> Int -> AST -> AST
  runFn' runFn_ res n = convert where
    convert :: AST -> AST
    convert js = fromMaybe js $ go n [] js

    go :: Int -> [AST] -> AST -> Maybe AST
    go 0 acc (App ss (Ref runFnN) [fn]) | isNFn runFn_ n runFnN && length acc == n =
      Just $ res ss fn acc
    go m acc (App _ lhs [arg]) = go (m - 1) (arg : acc) lhs
    go _ _   _ = Nothing

  inlineNonClassFunction :: (AST -> Bool) -> (AST -> AST -> AST) -> AST -> AST
  inlineNonClassFunction p f = convert where
    convert :: AST -> AST
    convert (App _ (App _ op' [x]) [y]) | p op' = f x y
    convert other = other

  isModFnWithDict :: (ModuleName, PSString) -> AST -> Bool
  isModFnWithDict fn (App _ (Ref fn') [Var _ _]) = fn == fn'
  isModFnWithDict _ _ = False

-- (f <<< g $ x) = f (g x)
-- (f <<< g)     = \x -> f (g x)
inlineFnComposition :: forall m. MonadSupply m => (AST -> AST) -> AST -> m AST
inlineFnComposition expander = everywhereTopDownM convert
  where
  convert :: AST -> m AST
  convert (App s1 (App s2 (App _ (expander -> App _ (Ref fn) [Ref CLibs.P_semigroupoidFn]) [x]) [y]) [z])
    | CLibs.P_compose <- fn = return $ App s1 x [App s2 y [z]]
    | CLibs.P_composeFlipped <- fn = return $ App s2 y [App s1 x [z]]
  convert app@(App ss (App _ (expander -> App _ (Ref fn) [Ref CLibs.P_semigroupoidFn]) _) _)
    | fn `elem` [CLibs.P_compose, CLibs.P_composeFlipped] = mkApps ss <$> goApps app <*> freshName
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
  goApps (App _ (App _ (expander -> App _ (Ref fn) [Ref CLibs.P_semigroupoidFn]) [x]) [y])
    | CLibs.P_compose <- fn = mappend <$> goApps x <*> goApps y
    | CLibs.P_composeFlipped <- fn = mappend <$> goApps y <*> goApps x
  goApps app@App {} = pure . Right . (,app) <$> freshName
  goApps other = pure [Left other]

inlineFnIdentity :: (AST -> AST) -> AST -> AST
inlineFnIdentity expander = everywhereTopDown convert
  where
  convert :: AST -> AST
  convert (App _ (expander -> App _ (Ref CLibs.P_identity) [Ref CLibs.P_categoryFn]) [x]) = x
  convert other = other

inlineUnsafeCoerce :: AST -> AST
inlineUnsafeCoerce = everywhereTopDown convert where
  convert (App _ (Ref CLibs.P_unsafeCoerce) [ comp ]) = comp
  convert other = other

inlineUnsafePartial :: AST -> AST
inlineUnsafePartial = everywhereTopDown convert where
  convert (App ss (Ref CLibs.P_unsafePartial) [ comp ])
    -- Apply to undefined here, the application should be optimized away
    -- if it is safe to do so
    = App ss comp [ Var ss CPrim.S_undefined ]
  convert other = other
