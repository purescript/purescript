module Language.PureScript.Sugar.Operators.Types where

import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common
import Language.PureScript.Types

matchTypeOperators
  :: forall a
   . Show a
  => [[(Qualified (OpName 'TypeOpName), a, Associativity)]]
  -> Type a
  -> Type a
matchTypeOperators = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Type a -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: Type a -> Maybe (Type a, Type a, Type a)
  extractOp (BinaryNoParensType op l r _) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Type a -> Maybe (Qualified (OpName 'TypeOpName), a)
  fromOp (TypeOp q@(Qualified _ (OpName _)) ann) = Just (q, ann)
  fromOp _ = Nothing

  reapply :: Qualified (OpName 'TypeOpName) -> a -> Type a -> Type a -> Type a
  reapply name ann l r = BinaryNoParensType (TypeOp name ann) l r ann
