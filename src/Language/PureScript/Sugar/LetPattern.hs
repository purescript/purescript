-- |
-- This module implements the desugaring pass which replaces patterns in let-in
-- expressions with appropriate case expressions.
--
module Language.PureScript.Sugar.LetPattern (desugarLetPatternModule) where

import Prelude.Compat
import Data.Function
import Data.List

import Language.PureScript.AST
import Language.PureScript.Crash

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
  replace (Let _ ds e) = go (partitionDecls ds) e
  replace other = other

  go :: [Either [Declaration] (SourceAnn, Binder, Expr)]
     -- ^ Declarations to desugar
     -> Expr
     -- ^ The original let-in result expression
     -> Expr
  go [] e = e
  go (Right (sa@(ss, _), binder, boundE) : ds) e =
    Case sa [boundE] [CaseAlternative ss [binder] [MkUnguarded ss $ go ds e]]
  go (Left d:ds) e = Let (declSourceAnn (head d)) d (go ds e)

partitionDecls :: [Declaration] -> [Either [Declaration] (SourceAnn, Binder, Expr)]
partitionDecls = concatMap f . groupBy ((==) `on` isBoundValueDeclaration)
  where
    f ds@(d:_)
      | isBoundValueDeclaration d = map (Right . g) ds
    f ds = [Left ds]

    g (BoundValueDeclaration sa binder expr) = (sa, binder, expr)
    g _ = internalError "partitionDecls: the impossible happened."

isBoundValueDeclaration :: Declaration -> Bool
isBoundValueDeclaration BoundValueDeclaration{} = True
isBoundValueDeclaration _ = False
