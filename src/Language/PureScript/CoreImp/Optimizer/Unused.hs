-- | Removes unused variables
module Language.PureScript.CoreImp.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUndefinedApp
  , removeUnusedPureVars
  ) where

import Prelude.Compat

import Control.Monad (filterM)
import Data.Monoid (Any(..))
import qualified Data.Set as S
import Data.Text (Text)

import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Optimizer.Common
import qualified Language.PureScript.Constants.Prim as C

removeCodeAfterReturnStatements :: AST -> AST
removeCodeAfterReturnStatements = everywhere (removeFromBlock go)
  where
  go :: [AST] -> [AST]
  go jss | not (any isReturn jss) = jss
         | otherwise = let (body, ret : _) = break isReturn jss in body ++ [ret]
  isReturn (Return _ _) = True
  isReturn (ReturnNoResult _) = True
  isReturn _ = False

removeUndefinedApp :: AST -> AST
removeUndefinedApp = everywhere convert
  where
  convert (App ss fn [Var _ arg]) | arg == C.undefined = App ss fn []
  convert js = js

removeUnusedPureVars :: [Text] -> [[AST]] -> [[AST]]
removeUnusedPureVars exps = loop
  where
  expsSet = S.fromList exps

  loop :: [[AST]] -> [[AST]]
  loop asts = if changed then loop (filter (not . null) asts') else asts
    where
    used = expsSet <> foldMap (foldMap (everything (<>) (\case Var _ x -> S.singleton x; _ -> S.empty))) asts
    (Any changed, asts') = traverse (filterM (anyFalses . isInUsedSet used)) asts

  isInUsedSet :: S.Set Text -> AST -> Bool
  isInUsedSet used = \case
    VariableIntroduction _ var (Just (IsPure, _)) -> var `S.member` used
    _ -> True

  anyFalses :: Bool -> (Any, Bool)
  anyFalses x = (Any (not x), x)
