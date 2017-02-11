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
desugarLetPatternModule :: Module a b -> Module a b
desugarLetPatternModule (Module ss coms mn ds exts) = Module ss coms mn (map desugarLetPattern ds) exts

-- |
-- Desugar a single let expression
--
desugarLetPattern :: Declaration a b -> Declaration a b
desugarLetPattern (PositionedDeclaration pos com d ann) = PositionedDeclaration pos com (desugarLetPattern d) ann
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr a b -> Expr a b
  replace (Let ds e) = go ds e
  replace other = other

  go :: [Declaration a b]
     -- ^ Declarations to desugar
     -> Expr a b
     -- ^ The original let-in result expression
     -> Expr a b
  go [] e = e
  go (pd@(PositionedDeclaration pos com d) : ds) e =
    case d of
      BoundValueDeclaration {} -> PositionedValue pos com $ go (d:ds) e
      _ -> append pd $ go ds e
  go (BoundValueDeclaration binder boundE : ds) e =
    Case [boundE] [CaseAlternative [binder] [MkUnguarded $ go ds e]]
  go (d:ds) e = append d $ go ds e

  append :: Declaration a b -> Expr a b -> Expr a b
  append d (Let ds e ann) = Let (d:ds) e ann
  append d e = Let [d] e
