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
import Language.PureScript.AST
import Language.PureScript.Errors

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error.Class

import Data.Monoid ((<>))
import Data.Function (on)
import Data.Functor.Identity
import Data.List (groupBy, sortBy)

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import qualified Language.PureScript.Constants as C

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket :: [Module] -> Either ErrorStack [Module]
rebracket ms = do
  let fixities = concatMap collectFixities ms
  ensureNoDuplicates $ map (\(i, pos, _) -> (i, pos)) fixities
  let opTable = customOperatorTable $ map (\(i, _, f) -> (i, f)) fixities
  mapM (rebracketModule opTable) ms

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module mn ds exts) = Module mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id

  go (UnaryMinus (NumericLiteral (Left n))) = NumericLiteral (Left $ negate n)
  go (UnaryMinus (NumericLiteral (Right n))) = NumericLiteral (Right $ negate n)
  go (UnaryMinus val) = App (Var (Qualified (Just (ModuleName [ProperName C.prelude])) (Ident C.negate))) val
  go other = other

rebracketModule :: [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]] -> Module -> Either ErrorStack Module
rebracketModule opTable (Module mn ds exts) =
  let (f, _, _) = everywhereOnValuesTopDownM return (matchOperators opTable) return
  in Module mn <$> (map removeParens <$> parU ds f) <*> pure exts

removeParens :: Declaration -> Declaration
removeParens =
  let (f, _, _) = everywhereOnValues id go id
  in f
  where
  go (Parens val) = val
  go val = val

collectFixities :: Module -> [(Qualified Ident, SourceSpan, Fixity)]
collectFixities (Module moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [(Qualified Ident, SourceSpan, Fixity)]
  collect (PositionedDeclaration pos _ (FixityDeclaration fixity name)) = [(Qualified (Just moduleName) (Op name), pos, fixity)]
  collect FixityDeclaration{} = error "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates :: [(Qualified Ident, SourceSpan)] -> Either ErrorStack ()
ensureNoDuplicates m = go $ sortBy (compare `on` fst) m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (Just mn) name), _) : (y, pos) : _) | x == y =
    rethrow (strMsg ("Error in module " ++ show mn) <>) $
      rethrowWithPosition pos $
        throwError $ mkErrorStack ("Redefined fixity for " ++ show name) Nothing
  go (_ : rest) = go rest

customOperatorTable :: [(Qualified Ident, Fixity)] -> [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]]
customOperatorTable fixities =
  let
    applyUserOp ident t1 = App (App (Var ident) t1)
    userOps = map (\(name, Fixity a p) -> (name, applyUserOp name, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, _, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
  in
    map (map (\(name, f, _, a) -> (name, f, a))) groups

type Chain = [Either Expr (Qualified Ident)]

matchOperators :: [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]] -> Expr -> Either ErrorStack Expr
matchOperators ops = parseChains
  where
  parseChains :: Expr -> Either ErrorStack Expr
  parseChains b@BinaryNoParens{} = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Expr -> Chain
  extendChain (BinaryNoParens name l r) = Left l : Right name : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain -> Either ErrorStack Expr
  bracketChain = either (Left . (`mkErrorStack` Nothing) . show) Right . P.parse (P.buildExpressionParser opTable parseValue <* P.eof) "operator expression"
  opTable = [P.Infix (P.try (parseTicks >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft]
            : map (map (\(name, f, a) -> P.Infix (P.try (matchOp name) >> return f) (toAssoc a))) ops
            ++ [[ P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft ]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t, Show t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token show (const (P.initialPos ""))

parseValue :: P.Parsec Chain () Expr
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

