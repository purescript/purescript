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

{-# LANGUAGE Rank2Types #-}

module Language.PureScript.Sugar.Operators (
  rebracket
) where

import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Values

import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Map as M
import qualified Data.Generics as G
import qualified Data.Generics.Extras as G
import Control.Monad.State
import Control.Applicative
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
  go m rb (Module name ds : ms) = do
    m' <- M.union m <$> collectFixities m (ModuleName name) ds
    let opTable = customOperatorTable m'
    ds' <- G.everywhereM' (G.mkM (matchOperators (ModuleName name) opTable)) ds
    go m' (Module name (G.everywhere (G.mkT removeParens) ds') : rb) ms

removeParens :: Value -> Value
removeParens (Parens val) = val
removeParens val = val

customOperatorTable :: M.Map (Qualified Ident) Fixity -> [[(Qualified Ident, Value -> Value -> Value, Associativity)]]
customOperatorTable fixities =
  let
    applyUserOp name t1 t2 = App (App (Var name) [t1]) [t2]
    userOps = map (\(name, Fixity a p) -> (name, applyUserOp name, p, a)) . M.toList $ fixities
    sorted = sortBy (compare `on` (\(_, _, p, _) -> p)) (userOps ++ builtIns)
    groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
  in
    map (map (\(name, f, _, a) -> (name, f, a))) groups

type Chain = [Either Value (Qualified Ident)]

matchOperators :: ModuleName -> [[(Qualified Ident, Value -> Value -> Value, Associativity)]] -> Value -> Either String Value
matchOperators moduleName ops val = G.everywhereM' (G.mkM parseChains) val
  where
  parseChains :: Value -> Either String Value
  parseChains b@(BinaryNoParens _ _ _) = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Value -> Chain
  extendChain (BinaryNoParens name l r) = Left l : Right name : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain -> Either String Value
  bracketChain = either (Left . show) Right . P.parse (P.buildExpressionParser opTable parseValue <* P.eof) "operator expression"
  opTable = map (map (\(name, f, a) -> P.Infix (P.try (matchOp moduleName name) >> return f) (toAssoc a))) ops
    ++ [[P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) [t1]) [t2]))) P.AssocLeft]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight

parseValue :: P.Parsec Chain () Value
parseValue = P.token show (const (P.initialPos "")) (either Just (const Nothing)) P.<?> "expression"

parseOp :: P.Parsec Chain () (Qualified Ident)
parseOp = P.token show (const (P.initialPos "")) (either (const Nothing) Just) P.<?> "operator"

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
collectFixities m moduleName (ImportDeclaration importedModule _ : rest) = do
  let fs = [ (i, fixity) | (Qualified mn i, fixity) <- M.toList m, mn == Just importedModule ]
  let m' = M.fromList (map (\(i, fixity) -> (Qualified (Just moduleName) i, fixity)) fs)
  collectFixities (M.union m' m) moduleName rest
collectFixities m moduleName (_:ds) = collectFixities m moduleName ds

globalOp :: String -> Qualified Ident
globalOp = Qualified Nothing . Op

builtIns :: [(Qualified Ident, Value -> Value -> Value, Precedence, Associativity)]
builtIns = [ (globalOp "<", Binary LessThan, 3, Infixl)
           , (globalOp "<=", Binary LessThanOrEqualTo, 3, Infixl)
           , (globalOp ">", Binary GreaterThan, 3, Infixl)
           , (globalOp ">=", Binary GreaterThanOrEqualTo, 3, Infixl)
           , (globalOp "!!", flip Indexer, 4, Infixl)
           , (globalOp "*", Binary Multiply, 5, Infixl)
           , (globalOp "/", Binary Divide, 5, Infixl)
           , (globalOp "%", Binary Modulus, 5, Infixl)
           , (globalOp "++", Binary Concat, 6, Infixr)
           , (globalOp "+", Binary Add, 7, Infixl)
           , (globalOp "-", Binary Subtract, 7, Infixl)
           , (globalOp "<<", Binary ShiftLeft, 8, Infixl)
           , (globalOp ">>", Binary ShiftRight, 8, Infixl)
           , (globalOp ">>>", Binary ZeroFillShiftRight, 8, Infixl)
           , (globalOp "==", Binary EqualTo, 9, Infixl)
           , (globalOp "!=", Binary NotEqualTo, 9, Infixl)
           , (globalOp "&", Binary BitwiseAnd, 10, Infixl)
           , (globalOp "^", Binary BitwiseXor, 10, Infixl)
           , (globalOp "|", Binary BitwiseOr, 10, Infixl)
           , (globalOp "&&", Binary And, 11, Infixr)
           , (globalOp "||", Binary Or, 11, Infixr)
           ]

