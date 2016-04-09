{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Operators.Expr where

import Prelude ()
import Prelude.Compat

import Data.Functor.Identity

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common

matchExprOperators :: [[(Qualified Ident, Associativity)]] -> Expr -> Expr
matchExprOperators = matchOperators isBinOp extractOp fromOp reapply modOpTable
  where

  isBinOp :: Expr -> Bool
  isBinOp BinaryNoParens{} = True
  isBinOp _ = False

  extractOp :: Expr -> Maybe (Expr, Expr, Expr)
  extractOp (BinaryNoParens op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Expr -> Maybe (Qualified Ident)
  fromOp (Var q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing

  reapply :: Qualified Ident -> Expr -> Expr -> Expr
  reapply op t1 t2 = App (App (Var op) t1) t2

  modOpTable
    :: [[P.Operator (Chain Expr) () Identity Expr]]
    -> [[P.Operator (Chain Expr) () Identity Expr]]
  modOpTable table =
    [ P.Infix (P.try (parseTicks >>= \op -> return (\t1 t2 -> App (App op t1) t2))) P.AssocLeft ]
    : table

  parseTicks :: P.Parsec (Chain Expr) () Expr
  parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
    where
    fromOther (Var (Qualified _ (Op _))) = Nothing
    fromOther v = Just v
