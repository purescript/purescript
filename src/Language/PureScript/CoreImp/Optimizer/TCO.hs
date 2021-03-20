-- | This module implements tail call elimination.
module Language.PureScript.CoreImp.Optimizer.TCO (tco) where

import Prelude.Compat

import Control.Applicative (empty, liftA2)
import Control.Monad (guard)
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (foldr)
import Data.Functor (($>), (<&>))
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.CoreImp.AST
import Language.PureScript.AST.SourcePos (SourceSpan)
import Safe (headDef, tailSafe)

-- | Eliminate tail calls
tco :: AST -> AST
tco = flip evalState 0 . everywhereTopDownM convert where
  tcoVar :: Text -> Text
  tcoVar arg = "$tco_var_" <> arg

  copyVar :: Text -> Text
  copyVar arg = "$copy_" <> arg

  tcoDoneM :: State Int Text
  tcoDoneM = get <&> \count -> "$tco_done" <>
    if count == 0 then "" else pack . show $ count

  tcoLoop :: Text
  tcoLoop = "$tco_loop"

  tcoResult :: Text
  tcoResult = "$tco_result"

  convert :: AST -> State Int AST
  convert (VariableIntroduction ss name (Just fn@Function {}))
      | Just trFns <- findTailRecursiveFns name arity body'
      = VariableIntroduction ss name . Just . replace <$> toLoop trFns name arity outerArgs innerArgs body'
    where
      innerArgs = headDef [] argss
      outerArgs = concat . reverse $ tailSafe argss
      arity = length argss
      -- ^ this is the number of calls, not the number of arguments, if there's
      -- ever a practical difference.
      (argss, body', replace) = topCollectAllFunctionArgs [] id fn
  convert js = pure js

  rewriteFunctionsWith :: ([Text] -> [Text]) -> [[Text]] -> (AST -> AST) -> AST -> ([[Text]], AST, AST -> AST)
  rewriteFunctionsWith argMapper = collectAllFunctionArgs
    where
    collectAllFunctionArgs allArgs f (Function s1 ident args (Block s2 (body@(Return _ _):_))) =
      collectAllFunctionArgs (args : allArgs) (\b -> f (Function s1 ident (argMapper args) (Block s2 [b]))) body
    collectAllFunctionArgs allArgs f (Function ss ident args body@(Block _ _)) =
      (args : allArgs, body, f . Function ss ident (argMapper args))
    collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args (Block s3 [body]))) =
      collectAllFunctionArgs (args : allArgs) (\b -> f (Return s1 (Function s2 ident (argMapper args) (Block s3 [b])))) body
    collectAllFunctionArgs allArgs f (Return s1 (Function s2 ident args body@(Block _ _))) =
      (args : allArgs, body, f . Return s1 . Function s2 ident (argMapper args))
    collectAllFunctionArgs allArgs f body = (allArgs, body, f)

  topCollectAllFunctionArgs :: [[Text]] -> (AST -> AST) -> AST -> ([[Text]], AST, AST -> AST)
  topCollectAllFunctionArgs = rewriteFunctionsWith (map copyVar)

  innerCollectAllFunctionArgs :: [[Text]] -> (AST -> AST) -> AST -> ([[Text]], AST, AST -> AST)
  innerCollectAllFunctionArgs = rewriteFunctionsWith id

  countReferences :: Text -> AST -> Int
  countReferences ident = everything (+) match where
    match :: AST -> Int
    match (Var _ ident') | ident == ident' = 1
    match _ = 0

  -- If `ident` is a tail-recursive function, returns a set of identifiers
  -- that are locally bound to functions participating in the tail recursion.
  -- Otherwise, returns Nothing.
  findTailRecursiveFns :: Text -> Int -> AST -> Maybe (S.Set Text)
  findTailRecursiveFns ident arity js = guard (countReferences ident js > 0) *> go (S.empty, S.singleton (ident, arity))
    where

    go :: (S.Set Text, S.Set (Text, Int)) -> Maybe (S.Set Text)
    go (known, required) =
      case S.minView required of
        Just (r, required') -> do
          required'' <- findTailPositionDeps r js
          go (S.insert (fst r) known, required' <> (S.filter (not . (`S.member` known) . fst) required''))
        Nothing ->
          pure known

  -- Returns set of identifiers (with their arities) that need to be used
  -- exclusively in tail calls using their full arity in order for this
  -- identifier to be considered in tail position (or Nothing if this
  -- identifier is used somewhere not as a tail call with full arity).
  findTailPositionDeps :: (Text, Int) -> AST -> Maybe (S.Set (Text, Int))
  findTailPositionDeps (ident, arity) js = allInTailPosition js where
    countSelfReferences = countReferences ident

    allInTailPosition (Return _ expr)
      | isSelfCall ident arity expr = guard (countSelfReferences expr == 1) $> S.empty
      | otherwise = guard (countSelfReferences expr == 0) $> S.empty
    allInTailPosition (While _ js1 body)
      = guard (countSelfReferences js1 == 0) *> allInTailPosition body
    allInTailPosition (For _ _ js1 js2 body)
      = guard (countSelfReferences js1 == 0 && countSelfReferences js2 == 0) *> allInTailPosition body
    allInTailPosition (ForIn _ _ js1 body)
      = guard (countSelfReferences js1 == 0) *> allInTailPosition body
    allInTailPosition (IfElse _ js1 body el)
      = guard (countSelfReferences js1 == 0) *> liftA2 mappend (allInTailPosition body) (foldMapA allInTailPosition el)
    allInTailPosition (Block _ body)
      = foldMapA allInTailPosition body
    allInTailPosition (Throw _ js1)
      = guard (countSelfReferences js1 == 0) $> S.empty
    allInTailPosition (ReturnNoResult _)
      = pure S.empty
    allInTailPosition (VariableIntroduction _ _ Nothing)
      = pure S.empty
    allInTailPosition (VariableIntroduction _ ident' (Just js1))
      | countSelfReferences js1 == 0 = pure S.empty
      | Function _ Nothing _ _ <- js1
      , (argss, body, _) <- innerCollectAllFunctionArgs [] id js1
        = S.insert (ident', length argss) <$> allInTailPosition body
      | otherwise = empty
    allInTailPosition (Assignment _ _ js1)
      = guard (countSelfReferences js1 == 0) $> S.empty
    allInTailPosition (Comment _ _ js1)
      = allInTailPosition js1
    allInTailPosition _
      = empty

  toLoop :: S.Set Text -> Text -> Int -> [Text] -> [Text] -> AST -> State Int AST
  toLoop trFns ident arity outerArgs innerArgs js = do
    tcoDone <- tcoDoneM
    modify (+ 1)

    let
      markDone :: Maybe SourceSpan -> AST
      markDone ss = Assignment ss (Var ss tcoDone) (BooleanLiteral ss True)

      loopify :: AST -> AST
      loopify (Return ss ret)
        | isSelfCall ident arity ret =
          let
            allArgumentValues = concat $ collectArgs [] ret
          in
            Block ss $
              zipWith (\val arg ->
                Assignment ss (Var ss (tcoVar arg)) val) allArgumentValues outerArgs
              ++ zipWith (\val arg ->
                Assignment ss (Var ss (copyVar arg)) val) (drop (length outerArgs) allArgumentValues) innerArgs
              ++ [ ReturnNoResult ss ]
        | isIndirectSelfCall ret = Return ss ret
        | otherwise = Block ss [ markDone ss, Return ss ret ]
      loopify (ReturnNoResult ss) = Block ss [ markDone ss, ReturnNoResult ss ]
      loopify (While ss cond body) = While ss cond (loopify body)
      loopify (For ss i js1 js2 body) = For ss i js1 js2 (loopify body)
      loopify (ForIn ss i js1 body) = ForIn ss i js1 (loopify body)
      loopify (IfElse ss cond body el) = IfElse ss cond (loopify body) (fmap loopify el)
      loopify (Block ss body) = Block ss (map loopify body)
      loopify (VariableIntroduction ss f (Just fn@(Function _ Nothing _ _)))
        | (_, body, replace) <- innerCollectAllFunctionArgs [] id fn
        , f `S.member` trFns = VariableIntroduction ss f (Just (replace (loopify body)))
      loopify other = other

    pure $ Block rootSS $
        map (\arg -> VariableIntroduction rootSS (tcoVar arg) (Just (Var rootSS (copyVar arg)))) outerArgs ++
        [ VariableIntroduction rootSS tcoDone (Just (BooleanLiteral rootSS False))
        , VariableIntroduction rootSS tcoResult Nothing
        , Function rootSS (Just tcoLoop) (outerArgs ++ innerArgs) (Block rootSS [loopify js])
        , While rootSS (Unary rootSS Not (Var rootSS tcoDone))
            (Block rootSS
              [(Assignment rootSS (Var rootSS tcoResult) (App rootSS (Var rootSS tcoLoop) ((map (Var rootSS . tcoVar) outerArgs) ++ (map (Var rootSS . copyVar) innerArgs))))])
        , Return rootSS (Var rootSS tcoResult)
        ]
    where
    rootSS = Nothing

    collectArgs :: [[AST]] -> AST -> [[AST]]
    collectArgs acc (App _ fn []) =
      -- count 0-argument applications as single-argument so we get the correct number of args
      collectArgs ([Var Nothing C.undefined] : acc) fn
    collectArgs acc (App _ fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

    isIndirectSelfCall :: AST -> Bool
    isIndirectSelfCall (App _ (Var _ ident') _) = ident' `S.member` trFns
    isIndirectSelfCall (App _ fn _) = isIndirectSelfCall fn
    isIndirectSelfCall _ = False

  isSelfCall :: Text -> Int -> AST -> Bool
  isSelfCall ident 1 (App _ (Var _ ident') _) = ident == ident'
  isSelfCall ident arity (App _ fn _) = isSelfCall ident (arity - 1) fn
  isSelfCall _ _ _ = False

foldMapA :: (Applicative f, Monoid w, Foldable t) => (a -> f w) -> t a -> f w
foldMapA f = foldr (liftA2 mappend . f) (pure mempty)
