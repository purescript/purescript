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
  rebracket
) where

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Values

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State

import Data.Function (on)
import Data.Functor.Identity
import Data.List (groupBy, sortBy)

import qualified Data.Map as M
import qualified Data.Generics as G
import qualified Data.Generics.Extras as G

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket :: [Module] -> Either String [Module]
rebracket = go M.empty []
  where
  go _ rb [] = return . reverse $ rb
  go m rb (Module name ds exps : ms) = do
    m' <- M.union m <$> collectFixities m name ds
    let opTable = customOperatorTable m'
    ds' <- G.everywhereM' (G.mkM (matchOperators name opTable)) ds
    go m' (Module name (G.everywhere (G.mkT removeParens) ds') exps : rb) ms

removeParens :: Value -> Value
removeParens (Parens val) = val
removeParens val = val

customOperatorTable :: M.Map (Qualified Ident) Fixity -> [[(Qualified Ident, Value -> Value -> Value, Associativity)]]
customOperatorTable fixities =
  let
    -- We make the assumption here that infix operators are not qualified. The parser currently enforces this.
    -- The fixity map can therefore map from module name/ident pairs to fixities, where the module name is the name
    -- of the module imported into, not from. This is useful in matchOp, but here we have to discard the module name to
    -- make sure that the generated code is correct.
    applyUserOp (Qualified _ name) t1 = App (App (Var (Qualified Nothing name)) t1)
    userOps = map (\(name, Fixity a p) -> (name, applyUserOp name, p, a)) . M.toList $ fixities
    sorted = sortBy (flip compare `on` (\(_, _, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
  in
    map (map (\(name, f, _, a) -> (name, f, a))) groups

type Chain = [Either Value (Qualified Ident)]

matchOperators :: ModuleName -> [[(Qualified Ident, Value -> Value -> Value, Associativity)]] -> Value -> Either String Value
matchOperators moduleName ops = G.everywhereM' (G.mkM parseChains)
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
            : map (map (\(name, f, a) -> P.Infix (P.try (matchOp moduleName name) >> return f) (toAssoc a))) ops
            ++ [[ P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft ]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight

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

matchOp :: ModuleName -> Qualified Ident -> P.Parsec Chain () ()
matchOp moduleName op = do
  ident <- parseOp
  guard (qualify moduleName ident == qualify moduleName op)

collectFixities :: M.Map (Qualified Ident) Fixity -> ModuleName -> [Declaration] -> Either String (M.Map (Qualified Ident) Fixity)
collectFixities m _ [] = return m
collectFixities m moduleName (FixityDeclaration fixity name : rest) = do
  let qual = Qualified (Just moduleName) (Op name)
  when (qual `M.member` m) (Left $ "redefined fixity for " ++ show name)
  collectFixities (M.insert qual fixity m) moduleName rest
collectFixities m moduleName (ImportDeclaration importedModule _ _ : rest) = do
  let fs = [ (i, fixity) | (Qualified mn i, fixity) <- M.toList m, mn == Just importedModule ]
  let m' = M.fromList (map (first (Qualified (Just moduleName))) fs)
  collectFixities (m' `M.union` m) moduleName rest
collectFixities m moduleName (_:ds) = collectFixities m moduleName ds

