-- |
-- This module implements the desugaring pass which replaces patterns in let-in
-- expressions with appropriate case expressions.
--
module Language.PureScript.Sugar.LetPattern (desugarLetPatternModule) where

import Prelude.Compat

import Language.PureScript.AST

-- |
-- Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
--
desugarLetPatternModule :: Module -> Module
desugarLetPatternModule (Module ss coms mn ds exts) =
  Module ss coms mn (fmap desugarLetPattern ds) exts

-- |
-- Desugar a single let expression
--
desugarLetPattern :: Declaration -> Declaration
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr -> Expr
  replace (Let _ ds e) = go ds e
  replace other = other

  go :: [Declaration]
     -- ^ Declarations to desugar
     -> Expr
     -- ^ The original let-in result expression
     -> Expr
  go [] e = e
  go (BoundValueDeclaration sa@(ss, _) binder boundE : ds) e =
    Case sa [boundE] [CaseAlternative ss [binder] [MkUnguarded ss $ go ds e]]
  go (d:ds) e = append d $ go ds e

  append :: Declaration -> Expr -> Expr
  append d (Let ss ds e) = Let ss (d:ds) e
  append d e = Let (declSourceAnn d) [d] e
