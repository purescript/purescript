-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Operators
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Language.PureScript.Sugar.Operators (
  rebracket,
  removeSignedLiterals
) where

import Language.PureScript.Names
import Language.PureScript.Declarations

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error.Class

import Data.Function (on)
import Data.Functor.Identity
import Data.List (sort, groupBy, sortBy)

import qualified Data.Data as D
import qualified Data.Generics as G
import qualified Data.Generics.Extras as G

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import qualified Language.PureScript.Constants as C

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket :: [Module] -> Either String [Module]
rebracket ms = do
  let fixities = concatMap collectFixities ms
  ensureNoDuplicates $ map fst fixities
  let opTable = customOperatorTable fixities
  mapM (rebracketModule opTable) ms

removeSignedLiterals :: (D.Data d) => d -> d
removeSignedLiterals = G.everywhere (G.mkT go)
  where
  go (UnaryMinus (NumericLiteral (Left n))) = NumericLiteral (Left $ negate n)
  go (UnaryMinus (NumericLiteral (Right n))) = NumericLiteral (Right $ negate n)
  go (UnaryMinus val) = App (Var (Qualified (Just (ModuleName [ProperName C.prelude])) (Ident C.negate))) val
  go other = other

rebracketModule :: [[(Qualified Ident, Value -> Value -> Value, Associativity)]] -> Module -> Either String Module
rebracketModule opTable (Module mn ds exts) = Module mn <$> (removeParens <$> G.everywhereM' (G.mkM (matchOperators opTable)) ds) <*> pure exts

removeParens :: (D.Data d) => d -> d
removeParens = G.everywhere (G.mkT go)
  where
  go (Parens val) = val
  go val = val

collectFixities :: Module -> [(Qualified Ident, Fixity)]
collectFixities (Module moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [(Qualified Ident, Fixity)]
  collect (PositionedDeclaration _ d) = collect d
  collect (FixityDeclaration fixity name) = [(Qualified (Just moduleName) (Op name), fixity)]
  collect _ = []

ensureNoDuplicates :: [Qualified Ident] -> Either String ()
ensureNoDuplicates m = go $ sort m
  where
  go [] = return ()
  go [_] = return ()
  go (x : y : _) | x == y = throwError $ "Redefined fixity for " ++ show x
  go (_ : rest) = go rest

customOperatorTable :: [(Qualified Ident, Fixity)] -> [[(Qualified Ident, Value -> Value -> Value, Associativity)]]
customOperatorTable fixities =
  let
    applyUserOp ident t1 = App (App (Var ident) t1)
    userOps = map (\(name, Fixity a p) -> (name, applyUserOp name, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, _, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
  in
    map (map (\(name, f, _, a) -> (name, f, a))) groups

type Chain = [Either Value (Qualified Ident)]

matchOperators :: [[(Qualified Ident, Value -> Value -> Value, Associativity)]] -> Value -> Either String Value
matchOperators ops = parseChains
  where
  parseChains :: Value -> Either String Value
  parseChains b@BinaryNoParens{} = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Value -> Chain
  extendChain (BinaryNoParens name l r) = Left l : Right name : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain -> Either String Value
  bracketChain = either (Left . show) Right . P.parse (P.buildExpressionParser opTable parseValue <* P.eof) "operator expression"
  opTable = [P.Infix (P.try (parseTicks >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft]
            : map (map (\(name, f, a) -> P.Infix (P.try (matchOp name) >> return f) (toAssoc a))) ops
            ++ [[ P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft ]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t, Show t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token show (const (P.initialPos ""))

parseValue :: P.Parsec Chain () Value
parseValue = token (either Just (const Nothing)) P.<?> "expression"

parseOp :: P.Parsec Chain () (Qualified Ident)
parseOp = token (either (const Nothing) fromOp) P.<?> "operator"
  where
  fromOp q@(Qualified _ (Op _)) = Just q
  fromOp _ = Nothing

parseTicks :: P.Parsec Chain () (Qualified Ident)
parseTicks = token (either (const Nothing) fromOp) P.<?> "infix function"
  where
  fromOp q@(Qualified _ (Ident _)) = Just q
  fromOp _ = Nothing

matchOp :: Qualified Ident -> P.Parsec Chain () ()
matchOp op = do
  ident <- parseOp
  guard $ ident == op

