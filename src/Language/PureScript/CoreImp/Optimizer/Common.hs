{-# LANGUAGE GADTs #-}

-- | Common functions used by the various optimizer phases
module Language.PureScript.CoreImp.Optimizer.Common where

import Prelude.Compat

import Data.Text (Text)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.CoreImp.AST
import Language.PureScript.PSString (PSString)

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id

replaceIdent :: forall ty ann. Text -> AST 'Expression ann -> AST ty ann -> AST ty ann
replaceIdent var1 js = everywhere replace where
  replace :: AST ty1 ann -> AST ty1 ann
  replace (Var _ var2) | var1 == var2 = js
  replace other = other

replaceIdents :: forall ty ann. [(Text, AST 'Expression ann)] -> AST ty ann -> AST ty ann
replaceIdents vars = everywhere replace where
  replace :: AST ty1 ann -> AST ty1 ann
  replace v@(Var _ var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: Text -> AST ty ann -> Bool
isReassigned var1 = everything (||) check where
  check :: AST ty ann -> Bool
  check (Function _ _ args _) | var1 `elem` args = True
  check (VariableIntroduction _ arg _) | var1 == arg = True
  check (Assignment _ (Var _ arg) _) | var1 == arg = True
  check (For _ arg _ _ _) | var1 == arg = True
  check (ForIn _ arg _ _) | var1 == arg = True
  check _ = False

isRebound :: AST ty1 ann -> AST ty ann -> Bool
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everything (++) variablesOf js) where
  variablesOf :: AST ty ann -> [Text]
  variablesOf (Var _ var) = [var]
  variablesOf _ = []

isUsed :: Text -> AST ty ann -> Bool
isUsed var1 = everything (||) check where
  check :: AST ty ann -> Bool
  check (Var _ var2) | var1 == var2 = True
  check (Assignment _ target _) | var1 == targetVariable target = True
  check _ = False

targetVariable :: AST ty ann -> Text
targetVariable (Var _ var) = var
targetVariable (Indexer _ _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: Text -> AST ty ann -> Bool
isUpdated var1 = everything (||) check where
  check :: AST ty ann -> Bool
  check (Assignment _ target _) | var1 == targetVariable target = True
  check _ = False

removeFromBlock :: ([AST ty ann] -> [AST ty ann]) -> AST ty ann -> AST ty ann
removeFromBlock go (Block ss sts) = Block ss (go sts)
removeFromBlock _  js = js

isDict :: (Text, PSString) -> AST ty ann -> Bool
isDict (moduleName, dictName) (Indexer _ (StringLiteral _ x) (Var _ y)) =
  x == dictName && y == moduleName
isDict _ _ = False

isDict' :: [(Text, PSString)] -> AST ty ann -> Bool
isDict' xs js = any (`isDict` js) xs
