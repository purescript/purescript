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
  :: forall m a b
   . MonadError MultipleErrors m
  => Module a b
  -> m (Module a b)
desugarTypeDeclarationsModule (Module ss coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $
    Module ss coms name <$> desugarTypeDeclarations ds <*> pure exps
  where

  desugarTypeDeclarations :: [Declaration a b] -> m [Declaration a b]
  desugarTypeDeclarations (PositionedDeclaration ann pos com d : rest) = do
    (d' : rest') <- rethrowWithPosition pos $ desugarTypeDeclarations (d : rest)
    return (PositionedDeclaration ann pos com d' : rest')
  desugarTypeDeclarations (TypeDeclaration ann name' ty : d : rest) = do
    (_, nameKind, val) <- fromValueDeclaration d
    desugarTypeDeclarations (ValueDeclaration ann name' nameKind [] [MkUnguarded (TypedValue ann True val ty)] : rest)
    where
    fromValueDeclaration :: Declaration a b -> m (Ident, NameKind, Expr a b)
    fromValueDeclaration (ValueDeclaration _ name'' nameKind [] [MkUnguarded val])
      | name' == name'' = return (name'', nameKind, val)
    fromValueDeclaration (PositionedDeclaration ann' pos com d') = do
      (ident, nameKind, val) <- rethrowWithPosition pos $ fromValueDeclaration d'
      return (ident, nameKind, PositionedValue ann' pos com val)
    fromValueDeclaration _ =
      throwError . errorMessage $ OrphanTypeDeclaration name'
  desugarTypeDeclarations [TypeDeclaration _ name' _] =
    throwError . errorMessage $ OrphanTypeDeclaration name'
  desugarTypeDeclarations (ValueDeclaration ann name' nameKind bs val : rest) = do
    let (_, f, _) = everywhereOnValuesTopDownM return go return
        f' = mapM (\(GuardedExpr g e) -> GuardedExpr g <$> f e)
    (:) <$> (ValueDeclaration ann name' nameKind bs <$> f' val)
        <*> desugarTypeDeclarations rest
    where
    go (Let ann' ds' val') = Let ann' <$> desugarTypeDeclarations ds' <*> pure val'
    go other = return other
  desugarTypeDeclarations (TypeInstanceDeclaration ann nm deps cls args (ExplicitInstance ds') : rest) =
    (:) <$> (TypeInstanceDeclaration ann nm deps cls args . ExplicitInstance <$> desugarTypeDeclarations ds')
        <*> desugarTypeDeclarations rest
  desugarTypeDeclarations (d:rest) = (:) d <$> desugarTypeDeclarations rest
  desugarTypeDeclarations [] = return []
