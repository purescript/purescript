-- |
-- This module implements the desugaring pass which replaces top-level type
-- declarations with type annotations on the corresponding expression.
--
module Language.PureScript.Sugar.TypeDeclarations
  ( desugarTypeDeclarationsModule
  ) where

import Prelude.Compat

import Control.Monad (unless)
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
  rethrow (addHint (ErrorInModule name)) $ do
    checkKindDeclarations ds
    checkRoleDeclarations Nothing ds
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
    go (Let w ds' val') = Let w <$> desugarTypeDeclarations ds' <*> pure val'
    go other = return other
  desugarTypeDeclarations (TypeInstanceDeclaration sa ch idx nm deps cls args (ExplicitInstance ds') : rest) =
    (:) <$> (TypeInstanceDeclaration sa ch idx nm deps cls args . ExplicitInstance <$> desugarTypeDeclarations ds')
        <*> desugarTypeDeclarations rest
  desugarTypeDeclarations (d:rest) = (:) d <$> desugarTypeDeclarations rest
  desugarTypeDeclarations [] = return []

  checkKindDeclarations :: [Declaration] -> m ()
  checkKindDeclarations (KindDeclaration sa kindFor name' _ : d : rest) = do
    unless (matchesDeclaration d) . throwError . errorMessage' (fst sa) $ OrphanKindDeclaration name'
    checkKindDeclarations rest
    where
    matchesDeclaration :: Declaration -> Bool
    matchesDeclaration (DataDeclaration _ Data name'' _ _) = kindFor == DataSig && name' == name''
    matchesDeclaration (DataDeclaration _ Newtype name'' _ _) = kindFor == NewtypeSig && name' == name''
    matchesDeclaration (TypeSynonymDeclaration _ name'' _ _) = kindFor == TypeSynonymSig && name' == name''
    matchesDeclaration (TypeClassDeclaration _ name'' _ _ _ _) = kindFor == ClassSig && name' == coerceProperName name''
    matchesDeclaration _ = False
  checkKindDeclarations (KindDeclaration sa _ name' _ : _) = do
    throwError . errorMessage' (fst sa) $ OrphanKindDeclaration name'
  checkKindDeclarations (_ : rest) = checkKindDeclarations rest
  checkKindDeclarations [] = return ()

  checkRoleDeclarations :: Maybe Declaration -> [Declaration] -> m ()
  checkRoleDeclarations Nothing (RoleDeclaration RoleDeclarationData{..} : _) =
    throwError . errorMessage' (fst rdeclSourceAnn) $ OrphanRoleDeclaration rdeclIdent
  checkRoleDeclarations (Just (RoleDeclaration (RoleDeclarationData _ name' _))) ((RoleDeclaration (RoleDeclarationData{..})) : _) | name' == rdeclIdent =
    throwError . errorMessage' (fst rdeclSourceAnn) $ DuplicateRoleDeclaration rdeclIdent
  checkRoleDeclarations (Just d) (rd@(RoleDeclaration (RoleDeclarationData{..})) : rest) = do
    unless (matchesDeclaration d) . throwError . errorMessage' (fst rdeclSourceAnn) $ OrphanRoleDeclaration rdeclIdent
    unless (isSupported d) . throwError . errorMessage' (fst rdeclSourceAnn) $ UnsupportedRoleDeclaration
    checkRoleDeclarationArity d
    checkRoleDeclarations (Just rd) rest
    where
    isSupported :: Declaration -> Bool
    isSupported (DataDeclaration{}) = True
    isSupported (ExternDataDeclaration{}) = True
    isSupported _ = False
    matchesDeclaration :: Declaration -> Bool
    matchesDeclaration (DataDeclaration _ _ name' _ _) = rdeclIdent == name'
    matchesDeclaration (ExternDataDeclaration _ name' _) = rdeclIdent == name'
    matchesDeclaration (TypeSynonymDeclaration _ name' _ _) = rdeclIdent == name'
    matchesDeclaration (TypeClassDeclaration _ name' _ _ _ _) = rdeclIdent == coerceProperName name'
    matchesDeclaration _ = False
    checkRoleDeclarationArity :: Declaration -> m ()
    checkRoleDeclarationArity (DataDeclaration _ _ _ args _) =
      throwRoleDeclarationArityMismatch $ length args
    checkRoleDeclarationArity (ExternDataDeclaration _ _ kind) =
      throwRoleDeclarationArityMismatch $ kindArity kind
    checkRoleDeclarationArity _ = return ()
    throwRoleDeclarationArityMismatch :: Int -> m ()
    throwRoleDeclarationArityMismatch expected = do
      let actual = length rdeclRoles
      unless (expected == actual) $
        throwError . errorMessage' (fst rdeclSourceAnn) $
          RoleDeclarationArityMismatch rdeclIdent expected actual
  checkRoleDeclarations _ (d : rest) = checkRoleDeclarations (Just d) rest
  checkRoleDeclarations _ [] = return ()
