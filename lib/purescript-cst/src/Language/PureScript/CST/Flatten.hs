module Language.PureScript.CST.Flatten where

import Prelude

import Data.DList (DList)
import Language.PureScript.CST.Types
import Language.PureScript.CST.Positions

flattenModule :: Module a -> DList SourceToken
flattenModule m@(Module _ a b c d e f g) =
  pure a <>
  flattenName b <>
  foldMap (flattenWrapped (flattenSeparated flattenExport)) c <>
  pure d <>
  foldMap flattenImportDecl e <>
  foldMap flattenDeclaration f <>
  pure (SourceToken (TokenAnn eofRange g []) TokEof)
  where
    (_, endTkn) = moduleRange m
    eofPos = advanceLeading (srcEnd (srcRange endTkn)) g
    eofRange = SourceRange eofPos eofPos

flattenDataHead :: DataHead a -> DList SourceToken
flattenDataHead (DataHead a b c) = pure a <> flattenName b <> foldMap flattenTypeVarBinding c

flattenDataCtor :: DataCtor a -> DList SourceToken
flattenDataCtor (DataCtor _ a b) = flattenName a <> foldMap flattenType b

flattenClassHead :: ClassHead a -> DList SourceToken
flattenClassHead (ClassHead a b c d e) =
  pure a <>
  foldMap (\(f, g) -> flattenOneOrDelimited flattenConstraint f <> pure g) b <>
  flattenName c <>
  foldMap flattenTypeVarBinding d <>
  foldMap (\(f, g) -> pure f <> flattenSeparated flattenClassFundep g) e

flattenClassFundep :: ClassFundep -> DList SourceToken
flattenClassFundep = \case
  FundepDetermined a b ->
    pure a <> foldMap flattenName b
  FundepDetermines a b c ->
    foldMap flattenName a <> pure b <> foldMap flattenName c

flattenInstance :: Instance a -> DList SourceToken
flattenInstance (Instance a b) =
  flattenInstanceHead a <> foldMap (\(c, d) -> pure c <> foldMap flattenInstanceBinding d) b

flattenInstanceHead :: InstanceHead a -> DList SourceToken
flattenInstanceHead (InstanceHead a b c d e f) =
  pure a <>
  flattenName b <>
  pure c <>
  foldMap (\(g, h) -> flattenOneOrDelimited flattenConstraint g <> pure h) d <>
  flattenQualifiedName e <>
  foldMap flattenType f

flattenInstanceBinding :: InstanceBinding a -> DList SourceToken
flattenInstanceBinding = \case
  InstanceBindingSignature _ a -> flattenLabeled flattenName flattenType a
  InstanceBindingName _ a -> flattenValueBindingFields a

flattenValueBindingFields :: ValueBindingFields a -> DList SourceToken
flattenValueBindingFields (ValueBindingFields a b c) =
  flattenName a <>
  foldMap flattenBinder b <>
  flattenGuarded c

flattenBinder :: Binder a -> DList SourceToken
flattenBinder = \case
  BinderWildcard _ a -> pure a
  BinderVar _ a -> flattenName a
  BinderNamed _ a b c -> flattenName a <> pure b <> flattenBinder c
  BinderConstructor _ a b -> flattenQualifiedName a <> foldMap flattenBinder b
  BinderBoolean _ a _ -> pure a
  BinderChar _ a _ -> pure a
  BinderString _ a _ -> pure a
  BinderNumber _ a b _ -> foldMap pure a <> pure b
  BinderArray _ a -> flattenWrapped (foldMap (flattenSeparated flattenBinder)) a
  BinderRecord _ a ->
    flattenWrapped (foldMap (flattenSeparated (flattenRecordLabeled flattenBinder))) a
  BinderParens _ a -> flattenWrapped flattenBinder a
  BinderTyped _ a b c -> flattenBinder a <> pure b <> flattenType c
  BinderOp _ a b c -> flattenBinder a <> flattenQualifiedName b <> flattenBinder c

flattenRecordLabeled :: (a -> DList SourceToken) -> RecordLabeled a -> DList SourceToken
flattenRecordLabeled f = \case
  RecordPun a -> flattenName a
  RecordField a b c -> flattenLabel a <> pure b <> f c

flattenRecordAccessor :: RecordAccessor a -> DList SourceToken
flattenRecordAccessor (RecordAccessor a b c) =
  flattenExpr a <> pure b <> flattenSeparated flattenLabel c

flattenRecordUpdate :: RecordUpdate a -> DList SourceToken
flattenRecordUpdate = \case
  RecordUpdateLeaf a b c -> flattenLabel a <> pure b <> flattenExpr c
  RecordUpdateBranch a b ->
    flattenLabel a <> flattenWrapped (flattenSeparated flattenRecordUpdate) b

flattenLambda :: Lambda a -> DList SourceToken
flattenLambda (Lambda a b c d) =
  pure a <> foldMap flattenBinder b <> pure c <> flattenExpr d

flattenIfThenElse :: IfThenElse a -> DList SourceToken
flattenIfThenElse (IfThenElse a b c d e f) =
  pure a <> flattenExpr b <> pure c <> flattenExpr d <> pure e <> flattenExpr f

flattenCaseOf :: CaseOf a -> DList SourceToken
flattenCaseOf (CaseOf a b c d) =
  pure a <>
  flattenSeparated flattenExpr b <>
  pure c <>
  foldMap (\(e, f) -> flattenSeparated flattenBinder e <> flattenGuarded f) d

flattenLetIn :: LetIn a -> DList SourceToken
flattenLetIn (LetIn a b c d) =
  pure a <> foldMap flattenLetBinding b <> pure c <> flattenExpr d

flattenDoBlock :: DoBlock a -> DList SourceToken
flattenDoBlock (DoBlock a b) =
  pure a <> foldMap flattenDoStatement b

flattenAdoBlock :: AdoBlock a -> DList SourceToken
flattenAdoBlock (AdoBlock a b c d) =
  pure a <> foldMap flattenDoStatement b <> pure c <> flattenExpr d

flattenDoStatement :: DoStatement a -> DList SourceToken
flattenDoStatement = \case
  DoLet a b -> pure a <> foldMap flattenLetBinding b
  DoDiscard a -> flattenExpr a
  DoBind a b c -> flattenBinder a <> pure b <> flattenExpr c

flattenExpr :: Expr a -> DList SourceToken
flattenExpr = \case
  ExprHole _ a -> flattenName a
  ExprSection _ a -> pure a
  ExprIdent _ a -> flattenQualifiedName a
  ExprConstructor _ a -> flattenQualifiedName a
  ExprBoolean _ a _ -> pure a
  ExprChar _ a _ -> pure a
  ExprString _ a _ -> pure a
  ExprNumber _ a _ -> pure a
  ExprArray _ a -> flattenWrapped (foldMap (flattenSeparated flattenExpr)) a
  ExprRecord _ a ->
    flattenWrapped (foldMap (flattenSeparated (flattenRecordLabeled flattenExpr))) a
  ExprParens _ a -> flattenWrapped flattenExpr a
  ExprTyped _ a b c -> flattenExpr a <> pure b <> flattenType c
  ExprInfix _ a b c -> flattenExpr a <> flattenWrapped flattenExpr b <> flattenExpr c
  ExprOp _ a b c -> flattenExpr a <> flattenQualifiedName b <> flattenExpr c
  ExprOpName _ a -> flattenQualifiedName a
  ExprNegate _ a b -> pure a <> flattenExpr b
  ExprRecordAccessor _ a -> flattenRecordAccessor a
  ExprRecordUpdate _ a b -> flattenExpr a <> flattenWrapped (flattenSeparated flattenRecordUpdate) b
  ExprApp _ a b -> flattenExpr a <> flattenExpr b
  ExprLambda _ a -> flattenLambda a
  ExprIf _ a -> flattenIfThenElse a
  ExprCase _ a -> flattenCaseOf a
  ExprLet _ a -> flattenLetIn a
  ExprDo _ a -> flattenDoBlock a
  ExprAdo _ a -> flattenAdoBlock a

flattenLetBinding :: LetBinding a -> DList SourceToken
flattenLetBinding = \case
  LetBindingSignature _ a -> flattenLabeled flattenName flattenType a
  LetBindingName _ a -> flattenValueBindingFields a
  LetBindingPattern _ a b c -> flattenBinder a <> pure b <> flattenWhere c

flattenWhere :: Where a -> DList SourceToken
flattenWhere (Where a b) =
  flattenExpr a <> foldMap (\(c, d) -> pure c <> foldMap flattenLetBinding d) b

flattenPatternGuard :: PatternGuard a -> DList SourceToken
flattenPatternGuard (PatternGuard a b) =
  foldMap (\(c, d) -> flattenBinder c <> pure d) a <> flattenExpr b

flattenGuardedExpr :: GuardedExpr a -> DList SourceToken
flattenGuardedExpr (GuardedExpr a b c d) =
  pure a <>
  flattenSeparated flattenPatternGuard b <>
  pure c <>
  flattenWhere d

flattenGuarded :: Guarded a -> DList SourceToken
flattenGuarded = \case
  Unconditional a b -> pure a <> flattenWhere b
  Guarded a -> foldMap flattenGuardedExpr a

flattenFixityFields :: FixityFields -> DList SourceToken
flattenFixityFields (FixityFields (a, _) (b, _) c) =
  pure a <> pure b <> flattenFixityOp c

flattenFixityOp :: FixityOp -> DList SourceToken
flattenFixityOp = \case
  FixityValue a b c -> flattenQualifiedName a <> pure b <> flattenName c
  FixityType a b c d -> pure a <> flattenQualifiedName b <> pure c <> flattenName d

flattenForeign :: Foreign a -> DList SourceToken
flattenForeign = \case
  ForeignValue a -> flattenLabeled flattenName flattenType a
  ForeignData a b -> pure a <> flattenLabeled flattenName flattenType b
  ForeignKind a b -> pure a <> flattenName b

flattenRole :: Role -> DList SourceToken
flattenRole = pure . roleTok

flattenDeclaration :: Declaration a -> DList SourceToken
flattenDeclaration = \case
  DeclData _ a b ->
    flattenDataHead a <>
    foldMap (\(t, cs) -> pure t <> flattenSeparated flattenDataCtor cs) b
  DeclType _ a b c ->flattenDataHead a <> pure b <> flattenType c
  DeclNewtype _ a b c d -> flattenDataHead a <> pure b <> flattenName c <> flattenType d
  DeclClass _ a b ->
    flattenClassHead a <>
    foldMap (\(c, d) -> pure c <> foldMap (flattenLabeled flattenName flattenType) d) b
  DeclInstanceChain _ a -> flattenSeparated flattenInstance a
  DeclDerive _ a b c -> pure a <> foldMap pure b <> flattenInstanceHead c
  DeclKindSignature _ a b -> pure a <> flattenLabeled flattenName flattenType b
  DeclSignature _ a -> flattenLabeled flattenName flattenType a
  DeclFixity _ a -> flattenFixityFields a
  DeclForeign _ a b c -> pure a <> pure b <> flattenForeign c
  DeclRole _ a b c d -> pure a <> pure b <> flattenName c <> foldMap flattenRole d
  DeclValue _ a -> flattenValueBindingFields a

flattenQualifiedName :: QualifiedName a -> DList SourceToken
flattenQualifiedName = pure . qualTok

flattenName :: Name a -> DList SourceToken
flattenName = pure . nameTok

flattenLabel :: Label -> DList SourceToken
flattenLabel = pure . lblTok

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

flattenOneOrDelimited
  :: (a -> DList SourceToken) -> OneOrDelimited a -> DList SourceToken
flattenOneOrDelimited f = \case
  One a -> f a
  Many a -> flattenWrapped (flattenSeparated f) a

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
