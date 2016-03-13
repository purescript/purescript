{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Sugar.Operators.Common where

import Prelude ()
import Prelude.Compat

import Control.Monad.State

import Data.Functor.Identity

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
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
  :: (a -> (Maybe (Qualified Ident)))
  -> P.Parsec (Chain a) () (Qualified Ident)
parseOp fromOp = token (either (const Nothing) fromOp) P.<?> "operator"

matchOp
  :: (a -> (Maybe (Qualified Ident)))
  -> Qualified Ident
  -> P.Parsec (Chain a) () ()
matchOp fromOp op = do
  ident <- parseOp fromOp
  guard $ ident == op

opTable
  :: [[(Qualified Ident, Associativity)]]
  -> (a -> Maybe (Qualified Ident))
  -> (Qualified Ident -> a -> a -> a)
  -> [[P.Operator (Chain a) () Identity a]]
opTable ops fromOp reapply =
  map (map (\(name, a) -> P.Infix (P.try (matchOp fromOp name) >> return (reapply name)) (toAssoc a))) ops
  ++ [[ P.Infix (P.try (parseOp fromOp >>= \ident -> return (reapply ident))) P.AssocLeft ]]
