module Language.PureScript.CST.Flatten where

import Prelude

import Data.DList (DList)
import Language.PureScript.CST.Types

flattenModule :: Module a -> DList SourceToken
flattenModule (Module _ a b c d e f g) =
  pure a <>
  flattenName b <>
  foldMap (flattenWrapped (flattenSeparated flattenExport)) c <>
  pure d <>
  foldMap flattenImportDecl e <>
  foldMap flattenDeclaration f <>
  pure (SourceToken (TokenAnn dummyRange g []) TokEof)
  where
    dummyPos = SourcePos 0 0
    dummyRange = SourceRange dummyPos dummyPos

flattenDeclaration :: Declaration a -> DList SourceToken
flattenDeclaration _ = mempty

flattenName :: Name a -> DList SourceToken
flattenName = pure . nameTok

flattenExport :: Export a -> DList SourceToken
flattenExport = \case
  ExportValue _ n -> flattenName n
  ExportOp _ n -> flattenName n
  ExportType _ n dms -> flattenName n <> foldMap flattenDataMembers dms
  ExportTypeOp _ t n -> pure t <> flattenName n
  ExportClass _ t n -> pure t <> flattenName n
  ExportKind _ t n -> pure t <> flattenName n
  ExportModule _ t n -> pure t <> flattenName n

flattenDataMembers :: DataMembers a -> DList SourceToken
flattenDataMembers = \case
  DataAll _ t -> pure t
  DataEnumerated _ ns -> flattenWrapped (foldMap (flattenSeparated flattenName)) ns

flattenImportDecl :: ImportDecl a -> DList SourceToken
flattenImportDecl (ImportDecl _ a b c d) =
  pure a <>
  flattenName b <>
  foldMap (\(mt, is) ->
             foldMap pure mt <> flattenWrapped (flattenSeparated flattenImport) is) c <>
  foldMap (\(t, n) -> pure t <> flattenName n) d

flattenImport :: Import a -> DList SourceToken
flattenImport = \case
  ImportValue _ n -> flattenName n
  ImportOp _ n -> flattenName n
  ImportType _ n dms -> flattenName n <> foldMap flattenDataMembers dms
  ImportTypeOp _ t n -> pure t <> flattenName n
  ImportClass _ t n -> pure t <> flattenName n
  ImportKind _ t n -> pure t <> flattenName n

flattenWrapped :: (a -> DList SourceToken) -> Wrapped a -> DList SourceToken
flattenWrapped k (Wrapped a b c) = pure a <> k b <> pure c

flattenSeparated :: (a -> DList SourceToken) -> Separated a -> DList SourceToken
flattenSeparated k (Separated a b) = k a <> foldMap (\(c, d) -> pure c <> k d) b

flattenLabeled :: (a -> DList SourceToken) -> (b -> DList SourceToken) -> Labeled a b -> DList SourceToken
flattenLabeled ka kc (Labeled a b c) = ka a <> pure b <> kc c

flattenType :: Type a -> DList SourceToken
flattenType = \case
  TypeVar _ a -> pure $ nameTok a
  TypeConstructor _ a -> pure $ qualTok a
  TypeWildcard _ a -> pure a
  TypeHole _ a -> pure $ nameTok a
  TypeString _ a _ -> pure a
  TypeRow _ a -> flattenWrapped flattenRow a
  TypeRecord _ a -> flattenWrapped flattenRow a
  TypeForall _ a b c d -> pure a <> foldMap flattenTypeVarBinding b <> pure c <> flattenType d
  TypeKinded _ a b c -> flattenType a <> pure b <> flattenType c
  TypeApp _ a b -> flattenType a <> flattenType b
  TypeOp _ a b c -> flattenType a <> pure (qualTok b) <> flattenType c
  TypeOpName _ a -> pure $ qualTok a
  TypeArr _ a b c -> flattenType a <> pure b <> flattenType c
  TypeArrName _ a -> pure a
  TypeConstrained _ a b c -> flattenConstraint a <> pure b <> flattenType c
  TypeParens _ a -> flattenWrapped flattenType a
  TypeUnaryRow _ a b -> pure a <> flattenType b

flattenRow :: Row a -> DList SourceToken
flattenRow (Row lbls tl) =
  foldMap (flattenSeparated (flattenLabeled (pure . lblTok) flattenType)) lbls
    <> foldMap (\(a, b) -> pure a <> flattenType b) tl

flattenTypeVarBinding :: TypeVarBinding a -> DList SourceToken
flattenTypeVarBinding = \case
  TypeVarKinded a -> flattenWrapped (flattenLabeled (pure . nameTok) flattenType) a
  TypeVarName a -> pure $ nameTok a

flattenConstraint :: Constraint a -> DList SourceToken
flattenConstraint = \case
  Constraint _ a b -> pure (qualTok a) <> foldMap flattenType b
  ConstraintParens _ a -> flattenWrapped flattenConstraint a
