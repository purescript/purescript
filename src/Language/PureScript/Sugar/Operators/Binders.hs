{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Operators.Binders where

import Prelude ()
import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Common

matchBinderOperators
  :: forall m
   . MonadError MultipleErrors m
  => [[(Qualified Ident, Associativity)]]
  -> Binder
  -> m Binder
matchBinderOperators ops = parseChains
  where
  parseChains :: Binder -> m Binder
  parseChains b@BinaryNoParensBinder{} = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Binder -> Chain Binder
  extendChain (BinaryNoParensBinder op l r) = Left l : Right op : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain Binder -> m Binder
  bracketChain =
    either
      (\_ -> internalError "matchBinderOperators: cannot reorder operators")
      return
    . P.parse opParser "operator expression"
  opParser = P.buildExpressionParser (opTable ops fromOp reapply) parseValue <* P.eof
  fromOp (OpBinder q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing
  reapply = BinaryNoParensBinder . OpBinder
