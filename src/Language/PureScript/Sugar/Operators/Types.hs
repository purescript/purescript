module Language.PureScript.Sugar.Operators.Types where

import Prelude.Compat

import Control.Monad.Except
import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common
import Language.PureScript.Types

matchTypeOperators
  :: MonadError MultipleErrors m
  => SourceSpan
  -> [[(Qualified (OpName 'TypeOpName), Associativity)]]
  -> Type
  -> m Type
matchTypeOperators ss = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Type -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: Type -> Maybe (Type, Type, Type)
  extractOp (BinaryNoParensType op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Type -> Maybe (SourceSpan, Qualified (OpName 'TypeOpName))
  fromOp (TypeOp q@(Qualified _ (OpName _))) = Just (ss, q)
  fromOp _ = Nothing

  reapply :: a -> Qualified (OpName 'TypeOpName) -> Type -> Type -> Type
  reapply _ = BinaryNoParensType . TypeOp
