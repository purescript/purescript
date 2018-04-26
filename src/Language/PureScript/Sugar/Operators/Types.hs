module Language.PureScript.Sugar.Operators.Types where

import Prelude.Compat

import Control.Monad.Except
import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common
import Language.PureScript.Types

matchTypeOperators
  :: MonadError MultipleErrors m
  => [[(Qualified (OpName 'TypeOpName), Associativity)]]
  -> Type
  -> m Type
matchTypeOperators = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Type -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: Type -> Maybe (Type, Type, Type)
  extractOp (BinaryNoParensType op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Type -> Maybe (a, Qualified (OpName 'TypeOpName))
  fromOp (TypeOp q@(Qualified _ (OpName _))) = Just (internalError "tried to use type operator source span", q)
  fromOp _ = Nothing

  reapply :: a -> Qualified (OpName 'TypeOpName) -> Type -> Type -> Type
  reapply _ = BinaryNoParensType . TypeOp
