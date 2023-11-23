-- | Removes unused variables
module Language.PureScript.CoreImp.Optimizer.Unused
  ( removeCodeAfterReturnStatements
  , removeUndefinedApp
  , removeUnusedEffectFreeVars
  ) where

import Prelude

import Control.Monad (filterM)
import Data.Monoid (Any(..))
import Data.Set qualified as S
import Data.Text (Text)

import Language.PureScript.CoreImp.AST (AST(..), InitializerEffects(..), everything, everywhere)
import Language.PureScript.CoreImp.Optimizer.Common (removeFromBlock)
import Language.PureScript.Constants.Prim qualified as C

removeCodeAfterReturnStatements :: AST -> AST
removeCodeAfterReturnStatements = everywhere (removeFromBlock go)
  where
  go :: [AST] -> [AST]
  go jss =
    case break isReturn jss of
      (_, []) -> jss
      (body, ret : _ ) -> body ++ [ret]

  isReturn (Return _ _) = True
  isReturn (ReturnNoResult _) = True
  isReturn _ = False

removeUndefinedApp :: AST -> AST
removeUndefinedApp = everywhere convert
  where
  convert (App ss fn [Var _ C.S_undefined]) = App ss fn []
  convert js = js

removeUnusedEffectFreeVars :: [Text] -> [[AST]] -> [[AST]]
removeUnusedEffectFreeVars exps = loop
  where
  expsSet = S.fromList exps

  loop :: [[AST]] -> [[AST]]
  loop asts = if changed then loop (filter (not . null) asts') else asts
    where
    used = expsSet <> foldMap (foldMap (everything (<>) (\case Var _ x -> S.singleton x; _ -> S.empty))) asts
    (Any changed, asts') = traverse (filterM (anyFalses . isInUsedSet used)) asts

  isInUsedSet :: S.Set Text -> AST -> Bool
  isInUsedSet used = \case
    VariableIntroduction _ var (Just (NoEffects, _)) -> var `S.member` used
    _ -> True

  anyFalses :: Bool -> (Any, Bool)
  anyFalses x = (Any (not x), x)
