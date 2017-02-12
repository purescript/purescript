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
desugarLetPattern :: forall a b. Declaration a b -> Declaration a b
desugarLetPattern (PositionedDeclaration pos com d ann) = PositionedDeclaration pos com (desugarLetPattern d) ann
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr a b -> Expr a b
  replace (Let ds e ann) = go ann ds e
  replace other = other

  go :: b
     -> [Declaration a b]
     -- ^ Declarations to desugar
     -> Expr a b
     -- ^ The original let-in result expression
     -> Expr a b
  go _ [] e = e
  go _ (pd@(PositionedDeclaration pos com d ann) : ds) e =
    case d of
      BoundValueDeclaration _ _ ann' -> PositionedValue pos com (go ann' (d:ds) e) ann
      _ -> append ann pd $ go ann ds e
  go _ (BoundValueDeclaration binder boundE ann : ds) e =
    Case [boundE] [CaseAlternative [binder] [MkUnguarded $ go ann ds e]] ann
  go ann (d:ds) e = append ann d $ go ann ds e

  append :: b -> Declaration a b -> Expr a b -> Expr a b
  append _ d (Let ds e ann) = Let (d:ds) e ann
  append ann d e = Let [d] e ann
