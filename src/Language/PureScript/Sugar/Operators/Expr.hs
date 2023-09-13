module Language.PureScript.Sugar.Operators.Expr where

import Prelude

import Control.Monad.Except (MonadError)
import Data.Functor.Identity (Identity)

import Text.Parsec qualified as P
import Text.Parsec.Expr qualified as P

import Language.PureScript.AST (Associativity, Expr(..), SourceSpan)
import Language.PureScript.Names (OpName(..), OpNameType(..), Qualified(..))
import Language.PureScript.Sugar.Operators.Common (Chain, matchOperators, token)
import Language.PureScript.Errors (MultipleErrors)

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
  reapply ss = BinaryNoParens . Op ss

  modOpTable
    :: [[P.Operator (Chain Expr) () Identity Expr]]
    -> [[P.Operator (Chain Expr) () Identity Expr]]
  modOpTable table =
    [ P.Infix (P.try (BinaryNoParens <$> parseTicks)) P.AssocLeft ]
    : table

  parseTicks :: P.Parsec (Chain Expr) () Expr
  parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
    where
    fromOther (Op _ _) = Nothing
    fromOther v = Just v
