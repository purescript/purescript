module Language.PureScript.Sugar.Operators.Expr where

import Prelude.Compat

import Data.Functor.Identity

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common

matchExprOperators
  :: forall a b
   . (Show a, Show b)
  => [[(Qualified (OpName 'ValueOpName), b, Associativity)]]
  -> Expr a b
  -> Expr a b
matchExprOperators = matchOperators isBinOp extractOp fromOp reapply modOpTable
  where

  isBinOp :: Expr a b -> Bool
  isBinOp BinaryNoParens{} = True
  isBinOp _ = False

  extractOp :: Expr a b -> Maybe (Expr a b, Expr a b, Expr a b)
  extractOp (BinaryNoParens _ op l r)
    | PositionedValue _ _ _ op' <- op = Just (op', l, r)
    | otherwise = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Expr a b -> Maybe (Qualified (OpName 'ValueOpName), b)
  fromOp (Op ann q@(Qualified _ (OpName _))) = Just (q, ann)
  fromOp _ = Nothing

  reapply :: Qualified (OpName 'ValueOpName) -> b -> Expr a b -> Expr a b -> Expr a b
  reapply op ann = App ann . App ann (Op ann op)

  modOpTable
    :: [[P.Operator (Chain (Expr a b)) () Identity (Expr a b)]]
    -> [[P.Operator (Chain (Expr a b)) () Identity (Expr a b)]]
  modOpTable table =
    [ P.Infix (P.try (parseTicks >>= \op ->
        let ann = extractExprAnn op
        in return (\t1 t2 -> App ann (App ann op t1) t2))) P.AssocLeft ]
    : table

  parseTicks :: P.Parsec (Chain (Expr a b)) () (Expr a b)
  parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
    where
    fromOther (Op _ _) = Nothing
    fromOther v = Just v
