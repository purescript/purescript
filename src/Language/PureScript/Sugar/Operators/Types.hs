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
  -> SourceType
  -> m SourceType
matchTypeOperators ss = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: SourceType -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: SourceType -> Maybe (SourceType, SourceType, SourceType)
  extractOp (BinaryNoParensType _ op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: SourceType -> Maybe (SourceSpan, Qualified (OpName 'TypeOpName))
  fromOp (TypeOp _ q@(Qualified _ (OpName _))) = Just (ss, q)
  fromOp _ = Nothing

  reapply :: a -> Qualified (OpName 'TypeOpName) -> SourceType -> SourceType -> SourceType
  reapply _ = srcBinaryNoParensType . srcTypeOp
