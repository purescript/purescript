-- | Common functions used by the various optimizer phases
module Language.PureScript.CoreImp.Optimizer.Common where

import Prelude

import Data.Text (Text)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Language.PureScript.Crash (internalError)
import Language.PureScript.CoreImp.AST (AST(..), everything, everywhere)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (PSString)

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id

replaceIdent :: Text -> AST -> AST -> AST
replaceIdent var1 js = everywhere replace
  where
  replace (Var _ var2) | var1 == var2 = js
  replace other = other

replaceIdents :: [(Text, AST)] -> AST -> AST
replaceIdents vars = everywhere replace
  where
  replace v@(Var _ var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: Text -> AST -> Bool
isReassigned var1 = everything (||) check
  where
  check :: AST -> Bool
  check (Function _ _ args _) | var1 `elem` args = True
  check (VariableIntroduction _ arg _) | var1 == arg = True
  check (Assignment _ (Var _ arg) _) | var1 == arg = True
  check (For _ arg _ _ _) | var1 == arg = True
  check (ForIn _ arg _ _) | var1 == arg = True
  check _ = False

isRebound :: AST -> AST -> Bool
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everything (++) variablesOf js)
  where
  variablesOf (Var _ var) = [var]
  variablesOf _ = []

targetVariable :: AST -> Text
targetVariable (Var _ var) = var
targetVariable (Indexer _ _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: Text -> AST -> Bool
isUpdated var1 = everything (||) check
  where
  check :: AST -> Bool
  check (Assignment _ target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([AST] -> [AST]) -> AST -> AST
removeFromBlock go (Block ss sts) = Block ss (go sts)
removeFromBlock _  js = js

pattern Ref :: (ModuleName, PSString) -> AST
pattern Ref pair <- (refPatternHelper -> Just pair)
-- ideally: pattern Ref (moduleName, refName) <- ModuleAccessor _ moduleName refName
-- but: https://gitlab.haskell.org/ghc/ghc/-/issues/12203
--      https://github.com/ghc-proposals/ghc-proposals/pull/138

refPatternHelper :: AST -> Maybe (ModuleName, PSString)
refPatternHelper = \case
  ModuleAccessor _ moduleName refName -> Just (moduleName, refName)
  _ -> Nothing
