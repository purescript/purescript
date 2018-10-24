module Language.PureScript.Sugar.Operators.Expr where

import Prelude.Compat

import Control.Monad.Except
import Data.Functor.Identity

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common
import Language.PureScript.Errors

matchExprOperators
  :: MonadError MultipleErrors m
  => [[(Qualified (OpName 'ValueOpName), Associativity)]]
  -> Expr
  -> m Expr
matchExprOperators = matchOperators isBinOp extractOp fromOp reapply modOpTable
  where

  isBinOp :: Expr -> Bool
  isBinOp BinaryNoParens{} = True
  isBinOp _ = False

  extractOp :: Expr -> Maybe (Expr, Expr, Expr)
  extractOp (BinaryNoParens op l r)
    | PositionedValue _ _ op' <- op = Just (op', l, r)
    | otherwise = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Expr -> Maybe (SourceSpan, Qualified (OpName 'ValueOpName))
  fromOp (Op ss q@(Qualified _ (OpName _))) = Just (ss, q)
  fromOp _ = Nothing

  reapply :: SourceSpan -> Qualified (OpName 'ValueOpName) -> Expr -> Expr -> Expr
  reapply ss op t1 = App (App (Op ss op) t1)

  modOpTable
    :: [[P.Operator (Chain Expr) () Identity Expr]]
    -> [[P.Operator (Chain Expr) () Identity Expr]]
  modOpTable table =
    [ P.Infix (P.try (parseTicks >>= \op -> return (\t1 t2 -> App (App op t1) t2))) P.AssocLeft ]
    : table

  parseTicks :: P.Parsec (Chain Expr) () Expr
  parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
    where
    fromOther (Op _ _) = Nothing
    fromOther v = Just v
