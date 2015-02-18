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
  removeSignedLiterals,
  desugarOperatorSections
) where

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Supply

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Function (on)
import Data.Functor.Identity
import Data.List (groupBy, sortBy)

import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import qualified Language.PureScript.Constants as C

type Chain = [Either Expr Expr]

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket :: [Module] -> Either ErrorStack [Module]
rebracket ms = do
  let fixities = concatMap collectFixities ms
  ensureNoDuplicates $ map (\(i, pos, _, _) -> (i, pos)) fixities
  let aliases = M.fromList $ mapMaybe (\(i, _, _, r) -> (,) i <$> r) fixities
  let opTable = customOperatorTable $ map (\(i, _, f, r) -> (i, f, r)) fixities
  mapM (rebracketModule aliases opTable) ms
  where
      
  collectFixities :: Module -> [(Qualified Ident, SourceSpan, Fixity, Maybe Ident)]
  collectFixities (Module moduleName ds _) = concatMap collect ds
    where
    collect :: Declaration -> [(Qualified Ident, SourceSpan, Fixity, Maybe Ident)]
    collect (PositionedDeclaration pos _ (FixityDeclaration fixity name repl)) = 
      [(Qualified (Just moduleName) (Op name), pos, fixity, Ident <$> repl)]
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
  
  customOperatorTable :: [(Qualified Ident, Fixity, Maybe Ident)] -> [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]]
  customOperatorTable fixities =
    let
      applyUserOp ident t1 = App (App (Var ident) t1)
      userOps = map (\(name@(Qualified mn name'), Fixity a p, repl) -> (name, applyUserOp (Qualified mn (fromMaybe name' repl)), p, a)) fixities
      sorted = sortBy (flip compare `on` (\(_, _, p, _) -> p)) userOps
      groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
    in
      map (map (\(name, f, _, a) -> (name, f, a))) groups

  rebracketModule :: M.Map (Qualified Ident) Ident -> [[(Qualified Ident, Expr -> Expr -> Expr, Associativity)]] -> Module -> Either ErrorStack Module
  rebracketModule aliases opTable (Module mn ds exts) =
    let (f, _, _) = everywhereOnValuesTopDownM return matchOperators return
    in Module mn <$> (map (removeParens . replaceFunctionNames) <$> parU ds f) <*> pure (map updateExports <$> exts)
    where
    removeParens :: Declaration -> Declaration
    removeParens =
      let (f, _, _) = everywhereOnValues id go id
      in f
      where
      go (Parens val) = val
      go val = val
      
    updateExports :: DeclarationRef -> DeclarationRef
    updateExports (ValueRef ident) = let ident' = M.lookup (Qualified (Just mn) ident) aliases 
                                     in ValueRef (fromMaybe ident ident')
    updateExports (PositionedDeclarationRef ss com ref) = PositionedDeclarationRef ss com (updateExports ref)
    updateExports other = other
    
    replaceFunctionNames :: Declaration -> Declaration
    replaceFunctionNames (TypeDeclaration ident ty) =
      let ident' = M.lookup (Qualified (Just mn) ident) aliases
      in TypeDeclaration (fromMaybe ident ident') ty
    replaceFunctionNames (ValueDeclaration ident a b c) =
      let ident' = M.lookup (Qualified (Just mn) ident) aliases
      in ValueDeclaration (fromMaybe ident ident') a b c
    replaceFunctionNames (PositionedDeclaration ss com d) = 
      PositionedDeclaration ss com (replaceFunctionNames d)
    replaceFunctionNames other = other
    
    matchOperators :: Expr -> Either ErrorStack Expr
    matchOperators = parseChains
      where
      parseChains :: Expr -> Either ErrorStack Expr
      parseChains b@BinaryNoParens{} = bracketChain (extendChain b)
      parseChains other = return other
      
      extendChain :: Expr -> Chain
      extendChain (BinaryNoParens op l r) = Left l : Right op : extendChain r
      extendChain other = [Left other]
      
      bracketChain :: Chain -> Either ErrorStack Expr
      bracketChain = either (Left . (`mkErrorStack` Nothing) . show) Right . P.parse (P.buildExpressionParser ops parseValue <* P.eof) "operator expression"
      
      ops = [P.Infix (P.try (parseTicks >>= \op -> return (\t1 t2 -> App (App op t1) t2))) P.AssocLeft]
            : map (map (\(name, f, a) -> P.Infix (P.try (matchOp name) >> return f) (toAssoc a))) opTable
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

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module mn ds exts) = Module mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id

  go (UnaryMinus val) = App (Var (Qualified (Just (ModuleName [ProperName C.prelude])) (Ident C.negate))) val
  go other = other

desugarOperatorSections :: Module -> SupplyT (Either ErrorStack) Module
desugarOperatorSections (Module mn ds exts) = Module mn <$> mapM goDecl ds <*> pure exts
  where

  goDecl :: Declaration -> SupplyT (Either ErrorStack) Declaration
  (goDecl, _, _) = everywhereOnValuesM return goExpr return

  goExpr :: Expr -> SupplyT (Either ErrorStack) Expr
  goExpr (OperatorSection op (Left val)) = return $ App op val
  goExpr (OperatorSection op (Right val)) = do
    arg <- Ident <$> freshName
    return $ Abs (Left arg) $ App (App op (Var (Qualified Nothing arg))) val
  goExpr other = return other
