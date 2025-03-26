-- |
-- Module for exhaustivity checking over pattern matching definitions
-- The algorithm analyses the clauses of a definition one by one from top
-- to bottom, where in each step it has the cases already missing (uncovered),
-- and it generates the new set of missing cases.
--
module Language.PureScript.Linter.Exhaustive
  ( checkExhaustiveExpr
  ) where

import Prelude
import Protolude (ordNub)

import Control.Arrow (first, second)
import Control.Monad (unless)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe)
import Data.Map qualified as M
import Data.Text qualified as T

import Language.PureScript.AST.Binders (Binder(..))
import Language.PureScript.AST.Declarations (CaseAlternative(..), Expr(..), Guard(..), GuardedExpr(..), pattern MkUnguarded, isTrueExpr)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.Traversals (everywhereOnValuesM)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType, Environment(..), TypeKind(..))
import Language.PureScript.Errors (MultipleErrors, pattern NullSourceAnn, SimpleErrorMessage(..), SourceSpan, errorMessage')
import Language.PureScript.Names as P
import Language.PureScript.Pretty.Values (prettyPrintBinderAtom)
import Language.PureScript.Types as P
import Language.PureScript.Constants.Prim qualified as C

-- | There are two modes of failure for the redundancy check:
--
-- 1. Exhaustivity was incomplete due to too many cases, so we couldn't determine redundancy.
-- 2. We didn't attempt to determine redundancy for a binder, e.g. an integer binder.
--
-- We want to warn the user in the first case.
data RedundancyError = Incomplete | Unknown

-- |
-- Qualifies a propername from a given qualified propername and a default module name
--
qualifyName
  :: ProperName a
  -> ModuleName
  -> Qualified (ProperName b)
  -> Qualified (ProperName a)
qualifyName n defmn qn = Qualified (ByModuleName mn) n
  where
  (mn, _) = qualify defmn qn

-- |
-- Given an environment and a datatype or newtype name,
-- this function returns the associated data constructors if it is the case of a datatype
-- where: - ProperName is the name of the constructor (for example, "Nothing" in Maybe)
--        - [Type] is the list of arguments, if it has (for example, "Just" has [TypeVar "a"])
--
getConstructors :: Environment -> ModuleName -> Qualified (ProperName 'ConstructorName) -> [(ProperName 'ConstructorName, [SourceType])]
getConstructors env defmn n = extractConstructors lnte
  where

  extractConstructors :: Maybe (SourceType, TypeKind) -> [(ProperName 'ConstructorName, [SourceType])]
  extractConstructors (Just (_, DataType _ _ pt)) = pt
  extractConstructors _ = internalError "Data name not in the scope of the current environment in extractConstructors"

  lnte :: Maybe (SourceType, TypeKind)
  lnte = M.lookup qpn (types env)

  qpn :: Qualified (ProperName 'TypeName)
  qpn = getConsDataName n

  getConsDataName :: Qualified (ProperName 'ConstructorName) -> Qualified (ProperName 'TypeName)
  getConsDataName con =
    case getConsInfo con of
      Nothing -> internalError $ "Constructor " ++ T.unpack (showQualified runProperName con) ++ " not in the scope of the current environment in getConsDataName."
      Just (_, pm, _, _) -> qualifyName pm defmn con

  getConsInfo :: Qualified (ProperName 'ConstructorName) -> Maybe (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
  getConsInfo con = M.lookup con (dataConstructors env)

-- |
-- Replicates a wildcard binder
--
initialize :: Int -> [Binder]
initialize l = replicate l NullBinder

-- |
-- Applies a function over two lists of tuples that may lack elements
--
genericMerge :: Ord a =>
  (a -> Maybe b -> Maybe c -> d) ->
  [(a, b)] ->
  [(a, c)] ->
  [d]
genericMerge _ [] [] = []
genericMerge f bs [] = map (\(s, b) -> f s (Just b) Nothing) bs
genericMerge f [] bs = map (\(s, b) -> f s Nothing (Just b)) bs
genericMerge f bsl@((s, b):bs) bsr@((s', b'):bs')
  | s < s' = f s (Just b) Nothing : genericMerge f bs bsr
  | s > s' = f s' Nothing (Just b') : genericMerge f bsl bs'
  | otherwise = f s (Just b) (Just b') : genericMerge f bs bs'

-- |
-- Find the uncovered set between two binders:
-- the first binder is the case we are trying to cover, the second one is the matching binder
--
missingCasesSingle :: Environment -> ModuleName -> Binder -> Binder -> ([Binder], Either RedundancyError Bool)
missingCasesSingle _ _ _ NullBinder = ([], return True)
missingCasesSingle _ _ _ (VarBinder _ _) = ([], return True)
missingCasesSingle env mn (VarBinder _ _) b = missingCasesSingle env mn NullBinder b
missingCasesSingle env mn br (NamedBinder _ _ bl) = missingCasesSingle env mn br bl
missingCasesSingle env mn NullBinder cb@(ConstructorBinder ss con _) =
  (concatMap (\cp -> fst $ missingCasesSingle env mn cp cb) allPatterns, return True)
  where
  allPatterns = map (\(p, t) -> ConstructorBinder ss (qualifyName p mn con) (initialize $ length t))
                  $ getConstructors env mn con
missingCasesSingle env mn cb@(ConstructorBinder ss con bs) (ConstructorBinder _ con' bs')
  | con == con' = let (bs'', pr) = missingCasesMultiple env mn bs bs' in (map (ConstructorBinder ss con) bs'', pr)
  | otherwise = ([cb], return False)
missingCasesSingle env mn NullBinder (LiteralBinder ss (ObjectLiteral bs)) =
  (map (LiteralBinder ss . ObjectLiteral . zip (map fst bs)) allMisses, pr)
  where
  (allMisses, pr) = missingCasesMultiple env mn (initialize $ length bs) (map snd bs)
missingCasesSingle env mn (LiteralBinder _ (ObjectLiteral bs)) (LiteralBinder ss (ObjectLiteral bs')) =
  (map (LiteralBinder ss . ObjectLiteral . zip sortedNames) allMisses, pr)
  where
  (allMisses, pr) = uncurry (missingCasesMultiple env mn) (unzip binders)

  sortNames = sortOn fst

  (sbs, sbs') = (sortNames bs, sortNames bs')

  compB :: a -> Maybe a -> Maybe a -> (a, a)
  compB e b b' = (fm b, fm b')
    where
    fm = fromMaybe e

  compBS :: b -> a -> Maybe b -> Maybe b -> (a, (b, b))
  compBS e s b b' = (s, compB e b b')

  (sortedNames, binders) = unzip $ genericMerge (compBS NullBinder) sbs sbs'
missingCasesSingle _ _ NullBinder (LiteralBinder ss (BooleanLiteral b)) = ([LiteralBinder ss . BooleanLiteral $ not b], return True)
missingCasesSingle _ _ (LiteralBinder ss (BooleanLiteral bl)) (LiteralBinder _ (BooleanLiteral br))
  | bl == br = ([], return True)
  | otherwise = ([LiteralBinder ss $ BooleanLiteral bl], return False)
missingCasesSingle env mn b (PositionedBinder _ _ cb) = missingCasesSingle env mn b cb
missingCasesSingle env mn b (TypedBinder _ cb) = missingCasesSingle env mn b cb
missingCasesSingle _ _ b _ = ([b], Left Unknown)

-- |
-- Returns the uncovered set of binders
-- the first argument is the list of uncovered binders at step i
-- the second argument is the (i+1)th clause of a pattern matching definition
--
-- The idea of the algorithm is as follows:
-- it processes each binder of the two lists (say, `x` and `y`) one by one
-- at each step two cases arises:
--   - there are no missing cases between `x` and `y`: this is very straightforward, it continues with the remaining cases
--       but keeps the uncovered binder in its position.
--   - there are missing cases, let us call it the set `U`: on the one hand, we mix each new uncovered case in `U`
--       with the current tail of uncovered set. On the other hand, it continues with the remaining cases: here we
--       can use `x` (but it will generate overlapping cases), or `y`, which will generate non-overlapping cases.
--
--     As an example, consider:
--
--       data N = Z | S N
--       f Z Z = Z   --> {[S _, _], [Z, S _]} which are the right non-overlapping cases (GHC uses this).
--
--       if we use `x` instead of `y` (in this case, `y` stands for `Z` and `x` for `_`) we obtain:
--       f Z Z = Z   --> {[S _, _], [_, S _]} you can see that both cases overlaps each other.
--
--       Up to now, we've decided to use `x` just because we expect to generate uncovered cases which might be
--       redundant or not, but uncovered at least. If we use `y` instead, we'll need to have a redundancy checker
--       (which ought to be available soon), or increase the complexity of the algorithm.
--
missingCasesMultiple :: Environment -> ModuleName -> [Binder] -> [Binder] -> ([[Binder]], Either RedundancyError Bool)
missingCasesMultiple env mn = go
  where
  go (x:xs) (y:ys) = (map (: xs) miss1 ++ map (x :) miss2, liftA2 (&&) pr1 pr2)
    where
    (miss1, pr1) = missingCasesSingle env mn x y
    (miss2, pr2) = go xs ys
  go _ _ = ([], pure True)

-- |
-- Guard handling
--
-- We say a guard is exhaustive iff it has an `otherwise` (or `true`) expression.
-- Example:
--   f x | x < 0 = 0
--       | otherwise = 1
--   is exhaustive, whereas `f x | x < 0` is not
--
-- or in case of a pattern guard if the pattern is exhaustive.
--
-- The function below say whether or not a guard has an `otherwise` expression
-- It is considered that `otherwise` is defined in Prelude
--
isExhaustiveGuard :: Environment -> ModuleName -> [GuardedExpr] -> Bool
isExhaustiveGuard _ _ [MkUnguarded _] = True
isExhaustiveGuard env moduleName gs   =
  any (\(GuardedExpr grd _) -> isExhaustive grd) gs
  where
    isExhaustive :: [Guard] -> Bool
    isExhaustive = all checkGuard

    checkGuard :: Guard -> Bool
    checkGuard (ConditionGuard cond)   = isTrueExpr cond
    checkGuard (PatternGuard binder _) =
      case missingCasesMultiple env moduleName [NullBinder] [binder] of
        ([], _) -> True -- there are no missing pattern for this guard
        _       -> False

-- |
-- Returns the uncovered set of case alternatives
--
missingCases :: Environment -> ModuleName -> [Binder] -> CaseAlternative -> ([[Binder]], Either RedundancyError Bool)
missingCases env mn uncovered ca = missingCasesMultiple env mn uncovered (caseAlternativeBinders ca)

missingAlternative :: Environment -> ModuleName -> CaseAlternative -> [Binder] -> ([[Binder]], Either RedundancyError Bool)
missingAlternative env mn ca uncovered
  | isExhaustiveGuard env mn (caseAlternativeResult ca) = mcases
  | otherwise = ([uncovered], snd mcases)
  where
  mcases = missingCases env mn uncovered ca

-- |
-- Main exhaustivity checking function
-- Starting with the set `uncovered = { _ }` (nothing covered, one `_` for each function argument),
-- it partitions that set with the new uncovered cases, until it consumes the whole set of clauses.
-- Then, returns the uncovered set of case alternatives.
--
checkExhaustive
  :: forall m
   . MonadWriter MultipleErrors m
   => SourceSpan
   -> Environment
   -> ModuleName
   -> Int
   -> [CaseAlternative]
   -> Expr
   -> m Expr
checkExhaustive ss env mn numArgs cas expr = makeResult . first ordNub $ foldl' step ([initialize numArgs], (pure True, [])) cas
  where
  step :: ([[Binder]], (Either RedundancyError Bool, [[Binder]])) -> CaseAlternative -> ([[Binder]], (Either RedundancyError Bool, [[Binder]]))
  step (uncovered, (nec, redundant)) ca =
    let (missed, pr) = unzip (map (missingAlternative env mn ca) uncovered)
        (missed', approx) = splitAt 10000 (ordNub (concat missed))
        cond = or <$> sequenceA pr
    in (missed', ( if null approx
                     then liftA2 (&&) cond nec
                     else Left Incomplete
                 , if and cond
                     then redundant
                     else caseAlternativeBinders ca : redundant
                 )
       )

  makeResult :: ([[Binder]], (Either RedundancyError Bool, [[Binder]])) -> m Expr
  makeResult (bss, (rr, bss')) =
    do unless (null bss') tellRedundant
       case rr of
         Left Incomplete -> tellIncomplete
         _ -> return ()
       return $ if null bss
         then expr
         else addPartialConstraint (second null (splitAt 5 bss)) expr
    where
      tellRedundant = tell . errorMessage' ss . uncurry OverlappingPattern . second null . splitAt 5 $ bss'
      tellIncomplete = tell . errorMessage' ss $ IncompleteExhaustivityCheck

  -- We add a Partial constraint by annotating the expression to have type `Partial => _`.
  --
  -- The binder information is provided so that it can be embedded in the constraint,
  -- and then included in the error message.
  addPartialConstraint :: ([[Binder]], Bool) -> Expr -> Expr
  addPartialConstraint (bss, complete) e =
    TypedValue True e $
      srcConstrainedType (srcConstraint C.Partial [] [] (Just constraintData)) $ TypeWildcard NullSourceAnn IgnoredWildcard
    where
      constraintData :: ConstraintData
      constraintData =
        PartialConstraintData (map (map prettyPrintBinderAtom) bss) complete

-- |
-- Exhaustivity checking
--
checkExhaustiveExpr
  :: forall m
   . MonadWriter MultipleErrors m
   => SourceSpan
   -> Environment
   -> ModuleName
   -> Expr
   -> m Expr
checkExhaustiveExpr ss env mn = onExpr'
  where
  (_, onExpr', _) = everywhereOnValuesM pure onExpr pure

  onExpr :: Expr -> m Expr
  onExpr e = case e of
    Case es cas ->
      checkExhaustive ss env mn (length es) cas e
    _ ->
      pure e
