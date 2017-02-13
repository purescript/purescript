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
desugarLetPattern (PositionedDeclaration ann pos com d) = PositionedDeclaration ann pos com (desugarLetPattern d)
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr a b -> Expr a b
  replace (Let ann ds e) = go ann ds e
  replace other = other

  go :: b
     -> [Declaration a b]
     -- ^ Declarations to desugar
     -> Expr a b
     -- ^ The original let-in result expression
     -> Expr a b
  go _ [] e = e
  go _ (pd@(PositionedDeclaration ann pos com d) : ds) e =
    case d of
      BoundValueDeclaration ann' _ _ -> PositionedValue ann pos com (go ann' (d:ds) e)
      _ -> append ann pd $ go ann ds e
  go _ (BoundValueDeclaration ann binder boundE : ds) e =
    Case ann [boundE] [CaseAlternative [binder] [MkUnguarded $ go ann ds e]]
  go ann (d:ds) e = append ann d $ go ann ds e

  append :: b -> Declaration a b -> Expr a b -> Expr a b
  append _ d (Let ann ds e) = Let ann (d:ds) e
  append ann d e = Let ann [d] e
