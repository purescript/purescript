module Language.PureScript.Sugar.Operators.Binders where

import Prelude ()
import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common

matchBinderOperators :: [[(Qualified Ident, Associativity)]] -> Binder -> Binder
matchBinderOperators = matchOperators isBinOp extractOp fromOp reapply id
  where

  isBinOp :: Binder -> Bool
  isBinOp BinaryNoParensBinder{} = True
  isBinOp _ = False

  extractOp :: Binder -> Maybe (Binder, Binder, Binder)
  extractOp (BinaryNoParensBinder op l r) = Just (op, l, r)
  extractOp _ = Nothing

  fromOp :: Binder -> Maybe (Qualified Ident)
  fromOp (OpBinder q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing

  reapply :: Qualified Ident -> Binder -> Binder -> Binder
  reapply = BinaryNoParensBinder . OpBinder
