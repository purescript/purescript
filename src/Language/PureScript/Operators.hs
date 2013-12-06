-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Operators
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Language.PureScript.Operators (
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

rebracket :: [Declaration] -> Either String [Declaration]
rebracket ds = do
  m <- collectFixities ds
  let opTable = customOperatorTable m
  ds' <- G.everywhereM' (G.mkM (matchOperators opTable)) ds
  return $ G.everywhere (G.mkT removeParens) ds'

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

matchOperators :: [[(Qualified Ident, Value -> Value -> Value, Associativity)]] -> Value -> Either String Value
matchOperators ops val = G.everywhereM' (G.mkM parseChains) val
  where
  parseChains :: Value -> Either String Value
  parseChains b@(BinaryNoParens _ _ _) = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Value -> Chain
  extendChain (BinaryNoParens name l r) = Left l : Right name : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain -> Either String Value
  bracketChain = either (Left . show) Right . P.parse (P.buildExpressionParser opTable parseValue <* P.eof) "operator expression"
  opTable = map (map (\(name, f, a) -> P.Infix (P.try (matchOp name) >> return f) (toAssoc a))) ops
    ++ [[P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) [t1]) [t2]))) P.AssocLeft]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight

parseValue :: P.Parsec Chain () Value
parseValue = P.token show (const (P.initialPos "")) (either Just (const Nothing)) P.<?> "expression"

parseOp :: P.Parsec Chain () (Qualified Ident)
parseOp = P.token show (const (P.initialPos "")) (either (const Nothing) Just) P.<?> "operator"

matchOp :: Qualified Ident -> P.Parsec Chain () ()
matchOp op = do
  ident <- parseOp
  guard (ident == op)

collectFixities :: [Declaration] -> Either String (M.Map (Qualified Ident) Fixity)
collectFixities = go M.empty global
  where
  go :: M.Map (Qualified Ident) Fixity -> ModulePath -> [Declaration] -> Either String (M.Map (Qualified Ident) Fixity)
  go m _ [] = return m
  go m p (FixityDeclaration fixity name : rest) = do
    let qual = Qualified p (Op name)
    when (qual `M.member` m) (Left $ "redefined fixity for " ++ show name)
    go (M.insert qual fixity m) p rest
  go m p (ModuleDeclaration name decls : rest) = do
    m' <- go m (subModule p name) decls
    go m' p rest
  go m p (_:ds) = go m p ds

globalOp :: String -> Qualified Ident
globalOp = Qualified global . Op

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

