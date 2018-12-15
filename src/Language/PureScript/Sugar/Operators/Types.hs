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
  -> Type SourceAnn
  -> m (Type SourceAnn)
matchTypeOperators ss = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Type SourceAnn -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: Type SourceAnn -> Maybe (Type SourceAnn, Type SourceAnn, Type SourceAnn)
  extractOp (BinaryNoParensType _ op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Type SourceAnn -> Maybe (SourceSpan, Qualified (OpName 'TypeOpName))
  fromOp (TypeOp _ q@(Qualified _ (OpName _))) = Just (ss, q)
  fromOp _ = Nothing

  reapply :: a -> Qualified (OpName 'TypeOpName) -> Type SourceAnn -> Type SourceAnn -> Type SourceAnn
  reapply _ = BinaryNoParensType NullSourceAnn . TypeOp NullSourceAnn
