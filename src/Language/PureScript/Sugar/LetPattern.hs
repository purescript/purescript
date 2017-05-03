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
desugarLetPatternModule (Module ss coms mn ds exts) = Module ss coms mn (map desugarLetPattern ds) exts

-- |
-- Desugar a single let expression
--
desugarLetPattern :: Declaration -> Declaration
desugarLetPattern (PositionedDeclaration pos com d) = PositionedDeclaration pos com $ desugarLetPattern d
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr -> Expr
  replace (Let ds e) = go ds e
  replace other = other

  go :: [Declaration]
     -- ^ Declarations to desugar
     -> Expr
     -- ^ The original let-in result expression
     -> Expr
  go [] e = e
  go (pd@(PositionedDeclaration pos com d) : ds) e =
    case d of
      BoundValueDeclaration {} -> PositionedValue pos com $ go (d:ds) e
      _ -> append pd $ go ds e
  go (BoundValueDeclaration binder boundE : ds) e =
    Case [boundE] [CaseAlternative [binder] [MkUnguarded $ go ds e]]
  go (d:ds) e = append d $ go ds e

  append :: Declaration -> Expr -> Expr
  append d (Let ds e) = Let (d:ds) e
  append d e = Let [d] e
