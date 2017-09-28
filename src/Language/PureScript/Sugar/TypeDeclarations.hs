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
desugarTypeDeclarationsModule (Module modSS coms name ds exps) =
  rethrow (addHint (ErrorInModule name)) $
    Module modSS coms name <$> desugarTypeDeclarations ds <*> pure exps
  where

  desugarTypeDeclarations :: [Declaration] -> m [Declaration]
  desugarTypeDeclarations (TypeDeclaration (TypeDeclarationData sa name' ty) : d : rest) = do
    (_, nameKind, val) <- fromValueDeclaration d
    desugarTypeDeclarations (ValueDecl sa name' nameKind [] [MkUnguarded (TypedValue True val ty)] : rest)
    where
    fromValueDeclaration :: Declaration -> m (Ident, NameKind, Expr)
    fromValueDeclaration (ValueDecl _ name'' nameKind [] [MkUnguarded val])
      | name' == name'' = return (name'', nameKind, val)
    fromValueDeclaration d' =
      throwError . errorMessage' (declSourceSpan d') $ OrphanTypeDeclaration name'
  desugarTypeDeclarations [TypeDeclaration (TypeDeclarationData (ss, _) name' _)] =
    throwError . errorMessage' ss $ OrphanTypeDeclaration name'
  desugarTypeDeclarations (ValueDecl sa name' nameKind bs val : rest) = do
    let (_, f, _) = everywhereOnValuesTopDownM return go return
        f' = mapM (\(GuardedExpr g e) -> GuardedExpr g <$> f e)
    (:) <$> (ValueDecl sa name' nameKind bs <$> f' val)
        <*> desugarTypeDeclarations rest
    where
    go (Let ds' val') = Let <$> desugarTypeDeclarations ds' <*> pure val'
    go other = return other
  desugarTypeDeclarations (TypeInstanceDeclaration sa ch idx nm deps cls args (ExplicitInstance ds') : rest) =
    (:) <$> (TypeInstanceDeclaration sa ch idx nm deps cls args . ExplicitInstance <$> desugarTypeDeclarations ds')
        <*> desugarTypeDeclarations rest
  desugarTypeDeclarations (d:rest) = (:) d <$> desugarTypeDeclarations rest
  desugarTypeDeclarations [] = return []
