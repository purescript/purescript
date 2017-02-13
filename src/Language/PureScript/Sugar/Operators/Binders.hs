module Language.PureScript.Sugar.Operators.Binders where

import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common

matchBinderOperators
  :: forall a b
   . (Show a, Show b)
  => [[(Qualified (OpName 'ValueOpName), b, Associativity)]]
  -> Binder a b
  -> Binder a b
matchBinderOperators = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Binder a b -> Bool
  isBinOp BinaryNoParensBinder{} = True
  isBinOp _ = False

  extractOp :: Binder a b -> Maybe (Binder a b, Binder a b, Binder a b)
  extractOp (BinaryNoParensBinder _ op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Binder a b -> Maybe (Qualified (OpName 'ValueOpName), b)
  fromOp (OpBinder ann q@(Qualified _ (OpName _))) = Just (q, ann)
  fromOp _ = Nothing

  reapply :: Qualified (OpName 'ValueOpName) -> b -> Binder a b -> Binder a b -> Binder a b
  reapply name ann = BinaryNoParensBinder ann (OpBinder ann name)
