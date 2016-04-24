module Language.PureScript.Sugar.Operators.Types where

import Prelude ()
import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common
import Language.PureScript.Types

matchTypeOperators :: [[(Qualified Ident, Associativity)]] -> Type -> Type
matchTypeOperators = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Type -> Bool
  isBinOp BinaryNoParensType{} = True
  isBinOp _ = False

  extractOp :: Type -> Maybe (Type, Type, Type)
  extractOp (BinaryNoParensType op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Type -> Maybe (Qualified Ident)
  fromOp (TypeOp q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing

  reapply :: Qualified Ident -> Type -> Type -> Type
  reapply = BinaryNoParensType . TypeOp
