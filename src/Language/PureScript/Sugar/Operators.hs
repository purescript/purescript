-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Operators
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.Operators (
  rebracket,
  removeSignedLiterals,
  desugarOperatorSections
) where

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Externs

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

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
rebracket :: (Applicative m, MonadError MultipleErrors m) => [ExternsFile] -> [Module] -> m [Module]
rebracket externs ms = do
  let fixities = concatMap externsFixities externs ++ concatMap collectFixities ms
  ensureNoDuplicates $ map (\(i, pos, _) -> (i, pos)) fixities
  let opTable = customOperatorTable $ map (\(i, _, f) -> (i, f)) fixities
  mapM (rebracketModule opTable) ms

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module ss coms mn ds exts) = Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id

  go (UnaryMinus val) = App (Var (Qualified (Just (ModuleName [ProperName C.prelude])) (Ident C.negate))) val
  go other = other

rebracketModule :: (Applicative m, MonadError MultipleErrors m) => [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]] -> Module -> m Module
rebracketModule opTable (Module ss coms mn ds exts) =
  let (f, _, _) = everywhereOnValuesTopDownM return (matchOperators opTable) return
  in Module ss coms mn <$> (map removeParens <$> parU ds f) <*> pure exts

removeParens :: Declaration -> Declaration
removeParens =
  let (f, _, _) = everywhereOnValues id go id
  in f
  where
  go (Parens val) = val
  go val = val

externsFixities :: ExternsFile -> [(Qualified Ident, SourceSpan, Fixity)]
externsFixities ExternsFile{..} =
  [ (Qualified (Just efModuleName) (Op op), internalModuleSourceSpan "", Fixity assoc prec)
  | ExternsFixity assoc prec op <- efFixities
  ]

collectFixities :: Module -> [(Qualified Ident, SourceSpan, Fixity)]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [(Qualified Ident, SourceSpan, Fixity)]
  collect (PositionedDeclaration pos _ (FixityDeclaration fixity name)) = [(Qualified (Just moduleName) (Op name), pos, fixity)]
  collect FixityDeclaration{} = internalError "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates :: (MonadError MultipleErrors m) => [(Qualified Ident, SourceSpan)] -> m ()
ensureNoDuplicates m = go $ sortBy (compare `on` fst) m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (Just mn) name), _) : (y, pos) : _) | x == y =
    rethrow (addHint (ErrorInModule mn)) $
      rethrowWithPosition pos $
        throwError . errorMessage $ MultipleFixities name
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

type Chain = [Either Expr Expr]

matchOperators :: forall m. (MonadError MultipleErrors m) => [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]] -> Expr -> m Expr
matchOperators ops = parseChains
  where
  parseChains :: Expr -> m Expr
  parseChains b@BinaryNoParens{} = bracketChain (extendChain b)
  parseChains other = return other
  extendChain :: Expr -> Chain
  extendChain (BinaryNoParens op l r) = Left l : Right op : extendChain r
  extendChain other = [Left other]
  bracketChain :: Chain -> m Expr
  bracketChain = either (\_ -> internalError "matchOperators: cannot reorder operators") return . P.parse (P.buildExpressionParser opTable parseValue <* P.eof) "operator expression"
  opTable = [P.Infix (P.try (parseTicks >>= \op -> return (\t1 t2 -> App (App op t1) t2))) P.AssocLeft]
            : map (map (\(name, f, a) -> P.Infix (P.try (matchOp name) >> return f) (toAssoc a))) ops
            ++ [[ P.Infix (P.try (parseOp >>= \ident -> return (\t1 t2 -> App (App (Var ident) t1) t2))) P.AssocLeft ]]

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token (const "") (const (P.initialPos ""))

parseValue :: P.Parsec Chain () Expr
parseValue = token (either Just (const Nothing)) P.<?> "expression"

parseOp :: P.Parsec Chain () (Qualified Ident)
parseOp = token (either (const Nothing) fromOp) P.<?> "operator"
  where
  fromOp (Var q@(Qualified _ (Op _))) = Just q
  fromOp _ = Nothing

parseTicks :: P.Parsec Chain () Expr
parseTicks = token (either (const Nothing) fromOther) P.<?> "infix function"
  where
  fromOther (Var (Qualified _ (Op _))) = Nothing
  fromOther v = Just v

matchOp :: Qualified Ident -> P.Parsec Chain () ()
matchOp op = do
  ident <- parseOp
  guard $ ident == op

desugarOperatorSections :: forall m. (Applicative m, MonadSupply m, MonadError MultipleErrors m) => Module -> m Module
desugarOperatorSections (Module ss coms mn ds exts) = Module ss coms mn <$> mapM goDecl ds <*> pure exts
  where

  goDecl :: Declaration -> m Declaration
  (goDecl, _, _) = everywhereOnValuesM return goExpr return

  goExpr :: Expr -> m Expr
  goExpr (OperatorSection op (Left val)) = return $ App op val
  goExpr (OperatorSection op (Right val)) = do
    arg <- Ident <$> freshName
    return $ Abs (Left arg) $ App (App op (Var (Qualified Nothing arg))) val
  goExpr other = return other
