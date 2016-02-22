{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Operators.Expr where

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

matchExprOperators
  :: forall m
   . MonadError MultipleErrors m
  => [[(Qualified Ident, Associativity)]]
  -> Expr
  -> m Expr
matchExprOperators ops = parseChains
  where
  parseChains :: Expr -> m Expr
  parseChains b@BinaryNoParens{} = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Expr -> Chain Expr
  extendChain (BinaryNoParens op l r) = Left l : Right op : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain Expr -> m Expr
  bracketChain =
    either
      (\_ -> internalError "matchExprOperators: cannot reorder operators")
      return
    . P.parse opParser "operator expression"
  opParser = P.buildExpressionParser opTable' parseValue <* P.eof
  opTable' =
    [ P.Infix (P.try (parseTicks >>= \op -> return (\t1 t2 -> App (App op t1) t2))) P.AssocLeft ]
    : opTable ops fromOp reapply
  fromOp (Var q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing
  reapply op t1 t2 = App (App (Var op) t1) t2

parseTicks :: P.Parsec (Chain Expr) () Expr
parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
  where
  fromOther (Var (Qualified _ (Op _))) = Nothing
  fromOther v = Just v
