module Language.PureScript.Sugar.Operators.Common where

import Prelude.Compat

import Control.Monad.State

import Data.Functor.Identity

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Names

type Chain a = [Either a a]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token (const "") (const (P.initialPos ""))

parseValue :: P.Parsec (Chain a) () a
parseValue = token (either Just (const Nothing)) P.<?> "expression"

parseOp
  :: (a -> (Maybe (Qualified (OpName nameType))))
  -> P.Parsec (Chain a) () (Qualified (OpName nameType))
parseOp fromOp = token (either (const Nothing) fromOp) P.<?> "operator"

matchOp
  :: (a -> (Maybe (Qualified (OpName nameType))))
  -> Qualified (OpName nameType)
  -> P.Parsec (Chain a) () ()
matchOp fromOp op = do
  ident <- parseOp fromOp
  guard $ ident == op

opTable
  :: [[(Qualified (OpName nameType), Associativity)]]
  -> (a -> Maybe (Qualified (OpName nameType)))
  -> (Qualified (OpName nameType) -> a -> a -> a)
  -> [[P.Operator (Chain a) () Identity a]]
opTable ops fromOp reapply =
  map (map (\(name, a) -> P.Infix (P.try (matchOp fromOp name) >> return (reapply name)) (toAssoc a))) ops
  ++ [[ P.Infix (P.try (parseOp fromOp >>= \ident -> return (reapply ident))) P.AssocLeft ]]

matchOperators
  :: forall a nameType
   . Show a
  => (a -> Bool)
  -> (a -> Maybe (a, a, a))
  -> (a -> Maybe (Qualified (OpName nameType)))
  -> (Qualified (OpName nameType) -> a -> a -> a)
  -> ([[P.Operator (Chain a) () Identity a]] -> P.OperatorTable (Chain a) () Identity a)
  -> [[(Qualified (OpName nameType), Associativity)]]
  -> a
  -> a
matchOperators isBinOp extractOp fromOp reapply modOpTable ops = parseChains
  where
  parseChains :: a -> a
  parseChains ty
    | True <- isBinOp ty = bracketChain (extendChain ty)
    | otherwise = ty
  extendChain :: a -> Chain a
  extendChain ty
    | Just (op, l, r) <- extractOp ty = Left l : Right op : extendChain r
    | otherwise = [Left ty]
  bracketChain :: Chain a -> a
  bracketChain =
    either
      (\_ -> internalError "matchTypeOperators: cannot reorder operators")
      id
    . P.parse opParser "operator expression"
  opParser = P.buildExpressionParser (modOpTable (opTable ops fromOp reapply)) parseValue <* P.eof
