-- |
-- This module implements the desugaring pass which replaces top-level type
-- declarations with type annotations on the corresponding expression.
--
module Language.PureScript.Sugar.TypeDeclarations
  ( desugarTypeDeclarationsModule
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all top level type declarations in a module with type annotations
--
desugarTypeDeclarationsModule
  :: forall m
   . MonadError MultipleErrors m
  => Module
  -> m Module
desugarTypeDeclarationsModule (Module ss coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $
    Module ss coms name <$> desugarTypeDeclarations ds <*> pure exps
  where

  desugarTypeDeclarations :: [Declaration] -> m [Declaration]
  desugarTypeDeclarations (PositionedDeclaration pos com d : rest) = do
    (d' : rest') <- rethrowWithPosition pos $ desugarTypeDeclarations (d : rest)
    return (PositionedDeclaration pos com d' : rest')
  desugarTypeDeclarations (TypeDeclaration name' ty : d : rest) = do
    (_, nameKind, val) <- fromValueDeclaration d
    desugarTypeDeclarations (ValueDeclaration name' nameKind [] [MkUnguarded (TypedValue True val ty)] : rest)
    where
    fromValueDeclaration :: Declaration -> m (Ident, NameKind, Expr)
    fromValueDeclaration (ValueDeclaration name'' nameKind [] [MkUnguarded val])
      | name' == name'' = return (name'', nameKind, val)
    fromValueDeclaration (PositionedDeclaration pos com d') = do
      (ident, nameKind, val) <- rethrowWithPosition pos $ fromValueDeclaration d'
      return (ident, nameKind, PositionedValue pos com val)
    fromValueDeclaration _ =
      throwError . errorMessage $ OrphanTypeDeclaration name'
  desugarTypeDeclarations [TypeDeclaration name' _] =
    throwError . errorMessage $ OrphanTypeDeclaration name'
  desugarTypeDeclarations (ValueDeclaration name' nameKind bs val : rest) = do
    let (_, f, _) = everywhereOnValuesTopDownM return go return
        f' = mapM (\(GuardedExpr g e) -> GuardedExpr g <$> f e)
    (:) <$> (ValueDeclaration name' nameKind bs <$> f' val)
        <*> desugarTypeDeclarations rest
    where
    go (Let ds' val') = Let <$> desugarTypeDeclarations ds' <*> pure val'
    go other = return other
  desugarTypeDeclarations (TypeInstanceDeclaration nm deps cls args (ExplicitInstance ds') : rest) =
    (:) <$> (TypeInstanceDeclaration nm deps cls args . ExplicitInstance <$> desugarTypeDeclarations ds')
        <*> desugarTypeDeclarations rest
  desugarTypeDeclarations (d:rest) = (:) d <$> desugarTypeDeclarations rest
  desugarTypeDeclarations [] = return []
