-- |
-- This module implements the desugaring pass which replaces patterns in let-in
-- expressions with appropriate case expressions.
--
module Language.PureScript.Sugar.LetPattern (desugarLetPatternModule) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Errors

-- |
-- Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
--
desugarLetPatternModule :: forall m. (MonadError MultipleErrors m) => Module -> m Module
desugarLetPatternModule (Module ss coms mn ds exts) = Module ss coms mn <$> parU ds desugarLetPattern <*> pure exts

-- |
-- Desugar a single let expression
--
desugarLetPattern :: forall m. (MonadError MultipleErrors m) => Declaration -> m Declaration
desugarLetPattern (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> rethrowWithPosition pos (desugarLetPattern d)
desugarLetPattern decl =
  let (f, _, _) = everywhereOnValuesM return replace return
  in f decl
  where
  replace :: Expr -> m Expr
  replace (Let ds e) = go ds [] e
  replace other = return other

  go :: [Declaration]
     -- ^ Declarations to desugar
     -> [Declaration]
     -- ^ Declarations already desugared, in reverse order
     -> Expr
     -- ^ The original result expression
     -> m Expr
  go [] [] e = return e
  go (PositionedDeclaration pos com (BoundValueDeclaration binder boundE) : ds) ds' e =
    rethrowWithPosition pos $ go [] ds' . PositionedValue pos com =<< desugared
    where
    desugared :: m Expr
    desugared = do
      resultE <- go ds [] e
      return $ Case [boundE] [CaseAlternative [binder] (Right resultE)]
  go (d:ds) ds' e = go ds (d:ds') e
  go [] ds' e = return $ Let (reverse ds') e
