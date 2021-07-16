-- | This module contains functions for converting the CST into the core AST. It
-- is mostly boilerplate, and does the job of resolving ranges for all the nodes
-- and attaching comments.

module Language.PureScript.CST.Convert
  ( convertType
  , convertExpr
  , convertBinder
  , convertDeclaration
  , convertImportDecl
  , convertModule
  , sourcePos
  , sourceSpan
  , comment
  , comments
  ) where

import Prelude hiding (take)

import Data.Bifunctor (bimap, first)
import Data.Char (toLower)
import Data.Foldable (foldl', toList)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromJust, mapMaybe)
import qualified Data.Text as Text
import qualified Language.PureScript.AST as AST
import Language.PureScript.AST.Declarations.ChainId (mkChainId)
import qualified Language.PureScript.AST.SourcePos as Pos
import qualified Language.PureScript.Comments as C
import Language.PureScript.Crash (internalError)
import qualified Language.PureScript.Environment as Env
import qualified Language.PureScript.Label as L
import qualified Language.PureScript.Names as N
import Language.PureScript.PSString (mkString, prettyPrintStringJS)
import qualified Language.PureScript.Types as T
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Print (printToken)
import Language.PureScript.CST.Types

comment :: Comment a -> Maybe C.Comment
comment = \case
  Comment t
    | "{-" `Text.isPrefixOf` t -> Just $ C.BlockComment $ Text.drop 2 $ Text.dropEnd 2 t
    | "--" `Text.isPrefixOf` t -> Just $ C.LineComment $ Text.drop 2 t
  _ -> Nothing

comments :: [Comment a] -> [C.Comment]
comments = mapMaybe comment

sourcePos :: SourcePos -> Pos.SourcePos
sourcePos (SourcePos line col) = Pos.SourcePos line col

sourceSpan :: String -> SourceRange -> Pos.SourceSpan
sourceSpan name (SourceRange start end) = Pos.SourceSpan name (sourcePos start) (sourcePos end)

widenLeft :: TokenAnn -> Pos.SourceAnn -> Pos.SourceAnn
widenLeft ann (sp, _) =
  ( Pos.widenSourceSpan (sourceSpan (Pos.spanName sp) $ tokRange ann) sp
  , comments $ tokLeadingComments ann
  )

sourceAnnCommented :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnnCommented fileName (SourceToken ann1 _) (SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , comments $ tokLeadingComments ann1
  )

sourceAnn :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnn fileName (SourceToken ann1 _) (SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , []
  )

sourceName :: String -> Name a -> Pos.SourceAnn
sourceName fileName a = sourceAnnCommented fileName (nameTok a) (nameTok a)

sourceQualName :: String -> QualifiedName a -> Pos.SourceAnn
sourceQualName fileName a = sourceAnnCommented fileName (qualTok a) (qualTok a)

moduleName :: Token -> Maybe N.ModuleName
moduleName = \case
  TokLowerName as _ -> go as
  TokUpperName as _ -> go as
  TokSymbolName as _ -> go as
  TokOperator as _ -> go as
  _ -> Nothing
  where
  go [] = Nothing
  go ns = Just $ N.ModuleName $ Text.intercalate "." ns

qualified :: QualifiedName a -> N.Qualified a
qualified q = N.Qualified (qualModule q) (qualName q)

ident :: Ident -> N.Ident
ident = N.Ident . getIdent

convertType :: String -> Type a -> T.SourceType
convertType fileName = go
  where
  goRow (Row labels tl) b = do
    let
      rowTail = case tl of
        Just (_, ty) -> go ty
        Nothing -> T.REmpty $ sourceAnnCommented fileName b b
      rowCons (Labeled a _ ty) c = do
        let ann = sourceAnnCommented fileName (lblTok a) (snd $ typeRange ty)
        T.RCons ann (L.Label $ lblName a) (go ty) c
    case labels of
      Just (Separated h t) ->
        rowCons h $ foldr (rowCons . snd) rowTail t
      Nothing ->
        rowTail

  go = \case
    TypeVar _ a ->
      T.TypeVar (sourceName fileName a) . getIdent $ nameValue a
    TypeConstructor _ a ->
      T.TypeConstructor (sourceQualName fileName a) $ qualified a
    TypeWildcard _ a ->
      T.TypeWildcard (sourceAnnCommented fileName a a) Nothing
    TypeHole _ a ->
      T.TypeWildcard (sourceName fileName a) . Just . getIdent $ nameValue a
    TypeString _ a b ->
      T.TypeLevelString (sourceAnnCommented fileName a a) b
    TypeRow _ (Wrapped _ row b) ->
      goRow row b
    TypeRecord _ (Wrapped a row b) -> do
      let
        ann = sourceAnnCommented fileName a b
        annRec = sourceAnn fileName a a
      T.TypeApp ann (Env.tyRecord $> annRec) $ goRow row b
    TypeForall _ kw bindings _ ty -> do
      let
        mkForAll a b t = do
          let ann' = widenLeft (tokAnn $ nameTok a) $ T.getAnnForType t
          T.ForAll ann' (getIdent $ nameValue a) b t Nothing
        k (TypeVarKinded (Wrapped _ (Labeled a _ b) _)) = mkForAll a (Just (go b))
        k (TypeVarName a) = mkForAll a Nothing
        ty' = foldr k (go ty) bindings
        ann = widenLeft (tokAnn kw) $ T.getAnnForType ty'
      T.setAnnForType ann ty'
    TypeKinded _ ty _ kd -> do
      let
        ty' = go ty
        kd' = go kd
        ann = Pos.widenSourceAnn (T.getAnnForType ty') (T.getAnnForType kd')
      T.KindedType ann ty' kd'
    TypeApp _ a b -> do
      let
        a' = go a
        b' = go b
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann a' b'
    ty@(TypeOp _ _ _ _) -> do
      let
        reassoc op b' a = do
          let
            a'  = go a
            op' = T.TypeOp (sourceQualName fileName op) $ qualified op
            ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
          T.BinaryNoParensType ann op' (go a) b'
        loop k = \case
          TypeOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      loop go ty
    TypeOpName _ op -> do
      let rng = qualRange op
      T.TypeOp (uncurry (sourceAnnCommented fileName) rng) (qualified op)
    TypeArr _ a arr b -> do
      let
        a' = go a
        b' = go b
        arr' = Env.tyFunction $> sourceAnnCommented fileName arr arr
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann (T.TypeApp ann arr' a') b'
    TypeArrName _ a ->
      Env.tyFunction $> sourceAnnCommented fileName a a
    TypeConstrained _ a _ b -> do
      let
        a' = convertConstraint fileName a
        b' = go b
        ann = Pos.widenSourceAnn (T.constraintAnn a') (T.getAnnForType b')
      T.ConstrainedType ann a' b'
    TypeParens _ (Wrapped a ty b) ->
      T.ParensInType (sourceAnnCommented fileName a b) $ go ty
    ty@(TypeUnaryRow _ _ a) -> do
      let
        a' = go a
        rng = typeRange ty
        ann = uncurry (sourceAnnCommented fileName) rng
      T.setAnnForType ann $ Env.kindRow a'

convertConstraint :: String -> Constraint a -> T.SourceConstraint
convertConstraint fileName = go
  where
  go = \case
    cst@(Constraint _ name args) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ constraintRange cst
      T.Constraint ann (qualified name) [] (convertType fileName <$> args) Nothing
    ConstraintParens _ (Wrapped _ c _) -> go c

convertGuarded :: String -> Guarded a -> [AST.GuardedExpr]
convertGuarded fileName = \case
  Unconditional _ x -> [AST.GuardedExpr [] (convertWhere fileName x)]
  Guarded gs -> (\(GuardedExpr _ ps _ x) -> AST.GuardedExpr (p <$> toList ps) (convertWhere fileName x)) <$> NE.toList gs
  where
  go = convertExpr fileName
  p (PatternGuard Nothing x) = AST.ConditionGuard (go x)
  p (PatternGuard (Just (b, _)) x) = AST.PatternGuard (convertBinder fileName b) (go x)

convertWhere :: String -> Where a -> AST.Expr
convertWhere fileName = \case
  Where expr Nothing -> convertExpr fileName expr
  Where expr (Just (_, bs)) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
    uncurry AST.PositionedValue ann . AST.Let AST.FromWhere (convertLetBinding fileName <$> NE.toList bs) $ convertExpr fileName expr

convertLetBinding :: String -> LetBinding a -> AST.Declaration
convertLetBinding fileName = \case
  LetBindingSignature _ lbl ->
    convertSignature fileName lbl
  binding@(LetBindingName _ fields) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    convertValueBindingFields fileName ann fields
  binding@(LetBindingPattern _ a _ b) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    AST.BoundValueDeclaration ann (convertBinder fileName a) (convertWhere fileName b)
  binding@(LetBindingType _ (DataHead _ a vars) _ bd) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    AST.TypeSynonymDeclaration ann (nameValue a) (goTypeVar <$> vars) (convertType fileName bd)
  binding@(LetBindingKindSignature _ _ (Labeled name _ ty)) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
    AST.KindDeclaration ann AST.TypeSynonymSig (nameValue name) $ convertType fileName ty
  where
  goTypeVar = \case
    TypeVarKinded (Wrapped _ (Labeled x _ y) _) -> (getIdent $ nameValue x, Just $ convertType fileName y)
    TypeVarName x -> (getIdent $ nameValue x, Nothing)

convertExpr :: forall a. String -> Expr a -> AST.Expr
convertExpr fileName = go
  where
  positioned =
    uncurry AST.PositionedValue

  goDoStatement = \case
    stmt@(DoLet _ as) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationLet $ convertLetBinding fileName <$> NE.toList as
    stmt@(DoDiscard a) -> do
      let ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationValue $ go a
    stmt@(DoBind a _ b) -> do
      let
        ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
        a' = convertBinder fileName a
        b' = go b
      uncurry AST.PositionedDoNotationElement ann $ AST.DoNotationBind a' b'

  go = \case
    ExprHole _ a ->
      positioned (sourceName fileName a) . AST.Hole . getIdent $ nameValue a
    ExprSection _ a ->
      positioned (sourceAnnCommented fileName a a) AST.AnonymousArgument
    ExprIdent _ a -> do
      let ann = sourceQualName fileName a
      positioned ann . AST.Var (fst ann) . qualified $ fmap ident a
    ExprConstructor _ a -> do
      let ann = sourceQualName fileName a
      positioned ann . AST.Constructor (fst ann) $ qualified a
    ExprBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.BooleanLiteral b
    ExprChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.CharLiteral b
    ExprString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) . AST.StringLiteral $ b
    ExprNumber _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.NumericLiteral b
    ExprArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ArrayLiteral vals
    ExprRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> (mkString . getIdent $ nameValue f, go . ExprIdent z $ QualifiedName (nameTok f) Nothing (nameValue f))
          RecordField f _ v -> (lblName f, go v)
        vals = case bs of
          Just (Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ObjectLiteral vals
    ExprParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.Parens $ go b
    expr@(ExprTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      positioned ann $ AST.TypedValue True a' b'
    expr@(ExprInfix _ a (Wrapped _ b _) c) -> do
      let ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      positioned ann $ AST.BinaryNoParens (go b) (go a) (go c)
    expr@(ExprOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ exprRange expr
        reassoc op b a = do
          let op' = AST.Op (sourceSpan fileName . toSourceRange $ qualRange op) $ qualified op
          AST.BinaryNoParens op' (go a) b
        loop k = \case
          ExprOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      positioned ann $ loop go expr
    ExprOpName _ op -> do
      let
        rng = qualRange op
        op' = AST.Op (sourceSpan fileName $ toSourceRange rng) $ qualified op
      positioned (uncurry (sourceAnnCommented fileName) rng) op'
    expr@(ExprNegate _ _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.UnaryMinus (fst ann) $ go b
    expr@(ExprRecordAccessor _ (RecordAccessor a _ (Separated h t))) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        field x f = AST.Accessor (lblName f) x
      positioned ann $ foldl' (\x (_, f) -> field x f) (field (go a) h) t
    expr@(ExprRecordUpdate _ a b) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        k (RecordUpdateLeaf f _ x) = (lblName f, AST.Leaf $ go x)
        k (RecordUpdateBranch f xs) = (lblName f, AST.Branch $ toTree xs)
        toTree (Wrapped _ xs _) = AST.PathTree . AST.AssocList . map k $ toList xs
      positioned ann . AST.ObjectUpdateNested (go a) $ toTree b
    expr@(ExprApp _ a b) -> do
      let ann = uncurry (sourceAnn fileName) $ exprRange expr
      positioned ann $ AST.App (go a) (go b)
    expr@(ExprLambda _ (Lambda _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann
        . AST.Abs (convertBinder fileName (NE.head as))
        . foldr (AST.Abs . convertBinder fileName) (go b)
        $ NE.tail as
    expr@(ExprIf _ (IfThenElse _ a _ b _ c)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann $ AST.IfThenElse (go a) (go b) (go c)
    expr@(ExprCase _ (CaseOf _ as _ bs)) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        as' = go <$> toList as
        bs' = uncurry AST.CaseAlternative . bimap (map (convertBinder fileName) . toList) (convertGuarded fileName) <$> NE.toList bs
      positioned ann $ AST.Case as' bs'
    expr@(ExprLet _ (LetIn _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Let AST.FromLet (convertLetBinding fileName <$> NE.toList as) $ go b
    -- expr@(ExprWhere _ (Where a _ bs)) -> do
    --   let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
    --   positioned ann . AST.Let AST.FromWhere (goLetBinding <$> bs) $ go a
    expr@(ExprDo _ (DoBlock kw stmts)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Do (moduleName $ tokValue kw) $ goDoStatement <$> NE.toList stmts
    expr@(ExprAdo _ (AdoBlock kw stms _ a)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Ado (moduleName $ tokValue kw) (goDoStatement <$> stms) $ go a

convertBinder :: String -> Binder a -> AST.Binder
convertBinder fileName = go
  where
  positioned =
    uncurry AST.PositionedBinder

  go = \case
    BinderWildcard _ a ->
      positioned (sourceAnnCommented fileName a a) AST.NullBinder
    BinderVar _ a -> do
      let ann = sourceName fileName a
      positioned ann . AST.VarBinder (fst ann) . ident $ nameValue a
    binder@(BinderNamed _ a _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.NamedBinder (fst ann) (ident $ nameValue a) $ go b
    binder@(BinderConstructor _ a bs) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.ConstructorBinder (fst ann) (qualified a) $ go <$> bs
    BinderBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.BooleanLiteral b
    BinderChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.CharLiteral b
    BinderString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) . AST.StringLiteral $ b
    BinderNumber _ n a b -> do
      let
        ann = sourceAnnCommented fileName a a
        b'
          | isJust n = bimap negate negate b
          | otherwise = b
      positioned ann . AST.LiteralBinder (fst ann) $ AST.NumericLiteral b'
    BinderArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ArrayLiteral vals
    BinderRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> (mkString . getIdent $ nameValue f, go $ BinderVar z f)
          RecordField f _ v -> (lblName f, go v)
        vals = case bs of
          Just (Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ObjectLiteral vals
    BinderParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.ParensInBinder $ go b
    binder@(BinderTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . toSourceRange $ binderRange binder, [])
      positioned ann $ AST.TypedBinder b' a'
    binder@(BinderOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ binderRange binder
        reassoc op b a = do
          let op' = AST.OpBinder (sourceSpan fileName . toSourceRange $ qualRange op) $ qualified op
          AST.BinaryNoParensBinder op' (go a) b
        loop k = \case
          BinderOp _ a op b -> loop (reassoc op (k b)) a
          binder' -> k binder'
      positioned ann $ loop go binder

convertDeclaration :: String -> Declaration a -> [AST.Declaration]
convertDeclaration fileName decl = case decl of
  DeclData _ (DataHead _ a vars) bd -> do
    let
      ctrs :: SourceToken -> DataCtor a -> [(SourceToken, DataCtor a)] -> [AST.DataConstructorDeclaration]
      ctrs st (DataCtor _ name fields) tl
        = AST.DataConstructorDeclaration (sourceAnnCommented fileName st (nameTok name)) (nameValue name) (zip ctrFields $ convertType fileName <$> fields)
        : (case tl of
            [] -> []
            (st', ctor) : tl' -> ctrs st' ctor tl'
          )
    pure $ AST.DataDeclaration ann Env.Data (nameValue a) (goTypeVar <$> vars) (maybe [] (\(st, Separated hd tl) -> ctrs st hd tl) bd)
  DeclType _ (DataHead _ a vars) _ bd ->
    pure $ AST.TypeSynonymDeclaration ann
      (nameValue a)
      (goTypeVar <$> vars)
      (convertType fileName bd)
  DeclNewtype _ (DataHead _ a vars) st x ys -> do
    let ctrs = [AST.DataConstructorDeclaration (sourceAnnCommented fileName st (snd $ declRange decl)) (nameValue x) [(head ctrFields, convertType fileName ys)]]
    pure $ AST.DataDeclaration ann Env.Newtype (nameValue a) (goTypeVar <$> vars) ctrs
  DeclClass _ (ClassHead _ sup name vars fdeps) bd -> do
    let
      goTyVar (TypeVarKinded (Wrapped _ (Labeled a _ _) _)) = nameValue a
      goTyVar (TypeVarName a) = nameValue a
      vars' = zip (toList $ goTyVar <$> vars) [0..]
      goName = fromJust . flip lookup vars' . nameValue
      goFundep (FundepDetermined _ bs) = Env.FunctionalDependency [] (goName <$> NE.toList bs)
      goFundep (FundepDetermines as _ bs) = Env.FunctionalDependency (goName <$> NE.toList as) (goName <$> NE.toList bs)
      goSig (Labeled n _ ty) = do
        let
          ty' = convertType fileName ty
          ann' = widenLeft (tokAnn $ nameTok n) $ T.getAnnForType ty'
        AST.TypeDeclaration $ AST.TypeDeclarationData ann' (ident $ nameValue n) ty'
    pure $ AST.TypeClassDeclaration ann
      (nameValue name)
      (goTypeVar <$> vars)
      (convertConstraint fileName <$> maybe [] (toList . fst) sup)
      (goFundep <$> maybe [] (toList . snd) fdeps)
      (goSig <$> maybe [] (NE.toList . snd) bd)
  DeclInstanceChain _ insts -> do
    let
      chainId = mkChainId fileName $ startSourcePos $ instKeyword $ instHead $ sepHead insts
      goInst ix inst@(Instance (InstanceHead _ nameSep ctrs cls args) bd) = do
        let ann' = uncurry (sourceAnnCommented fileName) $ instanceRange inst
        AST.TypeInstanceDeclaration ann' chainId ix
          (mkPartialInstanceName nameSep cls args)
          (convertConstraint fileName <$> maybe [] (toList . fst) ctrs)
          (qualified cls)
          (convertType fileName <$> args)
          (AST.ExplicitInstance $ goInstanceBinding <$> maybe [] (NE.toList . snd) bd)
    uncurry goInst <$> zip [0..] (toList insts)
  DeclDerive _ _ new (InstanceHead kw nameSep ctrs cls args) -> do
    let
      chainId = mkChainId fileName $ startSourcePos kw
      name' = mkPartialInstanceName nameSep cls args
      instTy
        | isJust new = AST.NewtypeInstance
        | otherwise = AST.DerivedInstance
    pure $ AST.TypeInstanceDeclaration ann chainId 0 name'
      (convertConstraint fileName <$> maybe [] (toList . fst) ctrs)
      (qualified cls)
      (convertType fileName <$> args)
      instTy
  DeclKindSignature _ kw (Labeled name _ ty) -> do
    let
      kindFor = case tokValue kw of
        TokLowerName [] "data" -> AST.DataSig
        TokLowerName [] "newtype" -> AST.NewtypeSig
        TokLowerName [] "type" -> AST.TypeSynonymSig
        TokLowerName [] "class" -> AST.ClassSig
        tok -> internalError $ "Invalid kind signature keyword " <> Text.unpack (printToken tok)
    pure . AST.KindDeclaration ann kindFor (nameValue name) $ convertType fileName ty
  DeclSignature _ lbl ->
    pure $ convertSignature fileName lbl
  DeclValue _ fields ->
    pure $ convertValueBindingFields fileName ann fields
  DeclFixity _ (FixityFields (_, kw) (_, prec) fxop) -> do
    let
      assoc =  case kw of
        Infix  -> AST.Infix
        Infixr -> AST.Infixr
        Infixl -> AST.Infixl
      fixity = AST.Fixity assoc prec
    pure $ AST.FixityDeclaration ann $ case fxop of
      FixityValue name _ op -> do
        Left $ AST.ValueFixity fixity (first ident <$> qualified name) (nameValue op)
      FixityType _ name _ op ->
        Right $ AST.TypeFixity fixity (qualified name) (nameValue op)
  DeclForeign _ _ _ frn ->
    pure $ case frn of
      ForeignValue (Labeled a _ b) ->
        AST.ExternDeclaration ann (ident $ nameValue a) $ convertType fileName b
      ForeignData _ (Labeled a _ b) ->
        AST.ExternDataDeclaration ann (nameValue a) $ convertType fileName b
      ForeignKind _ a ->
        AST.DataDeclaration ann Env.Data (nameValue a) [] []
  DeclRole _ _ _ name roles ->
    pure $ AST.RoleDeclaration $
      AST.RoleDeclarationData ann (nameValue name) (roleValue <$> NE.toList roles)
  where
  ann =
    uncurry (sourceAnnCommented fileName) $ declRange decl

  startSourcePos :: SourceToken -> Pos.SourcePos
  startSourcePos = sourcePos . srcStart . tokRange . tokAnn

  mkPartialInstanceName :: Maybe (Name Ident, SourceToken) -> QualifiedName (N.ProperName 'N.ClassName) -> [Type a] -> Either Text.Text N.Ident
  mkPartialInstanceName nameSep cls args =
    maybe (Left genName) (Right . ident . nameValue . fst) nameSep
    where
      -- truncate to 25 chars to reduce verbosity
      -- of name and still keep it readable
      -- name will be used to create a GenIdent
      -- in desugaring process
      genName :: Text.Text
      genName = Text.take 25 (className <> typeArgs)

      className :: Text.Text
      className
        = foldMap (uncurry Text.cons . first toLower)
        . Text.uncons
        . N.runProperName
        $ qualName cls

      typeArgs :: Text.Text
      typeArgs = foldMap argName args

      argName :: Type a -> Text.Text
      argName = \case
        -- These are only useful to disambiguate between overlapping instances
        -- but they’re disallowed outside of instance chains. Since we’re
        -- avoiding name collisions with unique identifiers anyway,
        -- we don't need to render this constructor.
        TypeVar{} -> ""
        TypeConstructor _ qn -> N.runProperName $ qualName qn
        TypeOpName _ qn -> N.runOpName $ qualName qn
        TypeString _ _ ps -> prettyPrintStringJS ps

        -- Typed holes are disallowed in instance heads
        TypeHole{} -> ""
        TypeParens _ t -> argName $ wrpValue t
        TypeKinded _ t1 _ t2 -> argName t1 <> argName t2
        TypeRecord _ _ -> "Record"
        TypeRow _ _ -> "Row"
        TypeArrName _ _ -> "Function"
        TypeWildcard{} -> "_"

        -- Polytypes are disallowed in instance heads
        TypeForall{} -> ""
        TypeApp _ t1 t2 -> argName t1 <> argName t2
        TypeOp _ t1 op t2 ->
          argName t1 <> N.runOpName (qualName op) <> argName t2
        TypeArr _ t1 _ t2 -> argName t1 <> "Function" <> argName t2
        TypeConstrained{} -> ""
        TypeUnaryRow{} -> "Row"

  goTypeVar = \case
    TypeVarKinded (Wrapped _ (Labeled x _ y) _) -> (getIdent $ nameValue x, Just $ convertType fileName y)
    TypeVarName x -> (getIdent $ nameValue x, Nothing)

  goInstanceBinding = \case
    InstanceBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(InstanceBindingName _ fields) -> do
      let ann' = uncurry (sourceAnnCommented fileName) $ instanceBindingRange binding
      convertValueBindingFields fileName ann' fields

convertSignature :: String -> Labeled (Name Ident) (Type a) -> AST.Declaration
convertSignature fileName (Labeled a _ b) = do
  let
    b' = convertType fileName b
    ann = widenLeft (tokAnn $ nameTok a) $ T.getAnnForType b'
  AST.TypeDeclaration $ AST.TypeDeclarationData ann (ident $ nameValue a) b'

convertValueBindingFields :: String -> Pos.SourceAnn -> ValueBindingFields a -> AST.Declaration
convertValueBindingFields fileName ann (ValueBindingFields a bs c) = do
  let
    bs' = convertBinder fileName <$> bs
    cs' = convertGuarded fileName c
  AST.ValueDeclaration $ AST.ValueDeclarationData ann (ident $ nameValue a) Env.Public bs' cs'

convertImportDecl
  :: String
  -> ImportDecl a
  -> (Pos.SourceAnn, N.ModuleName, AST.ImportDeclarationType, Maybe N.ModuleName)
convertImportDecl fileName decl@(ImportDecl _ _ modName mbNames mbQual) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ importDeclRange decl
    importTy = case mbNames of
      Nothing -> AST.Implicit
      Just (hiding, Wrapped _ imps _) -> do
        let imps' = convertImport fileName <$> toList imps
        if isJust hiding
          then AST.Hiding imps'
          else AST.Explicit imps'
  (ann, nameValue modName, importTy, nameValue . snd <$> mbQual)

convertImport :: String -> Import a -> AST.DeclarationRef
convertImport fileName imp = case imp of
  ImportValue _ a ->
    AST.ValueRef ann . ident $ nameValue a
  ImportOp _ a ->
    AST.ValueOpRef ann $ nameValue a
  ImportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped _ Nothing _)) -> Just []
        Just (DataEnumerated _ (Wrapped _ (Just idents) _)) ->
          Just . map nameValue $ toList idents
    AST.TypeRef ann (nameValue a) ctrs
  ImportTypeOp _ _ a ->
    AST.TypeOpRef ann $ nameValue a
  ImportClass _ _ a ->
    AST.TypeClassRef ann $ nameValue a
  ImportKind _ _ a ->
    AST.TypeRef ann (nameValue a) (Just [])
  where
  ann = sourceSpan fileName . toSourceRange $ importRange imp

convertExport :: String -> Export a -> AST.DeclarationRef
convertExport fileName export = case export of
  ExportValue _ a ->
    AST.ValueRef ann . ident $ nameValue a
  ExportOp _ a ->
    AST.ValueOpRef ann $ nameValue a
  ExportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped _ Nothing _)) -> Just []
        Just (DataEnumerated _ (Wrapped _ (Just idents) _)) ->
          Just . map nameValue $ toList idents
    AST.TypeRef ann (nameValue a) ctrs
  ExportTypeOp _ _ a ->
    AST.TypeOpRef ann $ nameValue a
  ExportClass _ _ a ->
    AST.TypeClassRef ann $ nameValue a
  ExportKind _ _ a ->
    AST.TypeRef ann (nameValue a) Nothing
  ExportModule _ _ a ->
    AST.ModuleRef ann (nameValue a)
  where
  ann = sourceSpan fileName . toSourceRange $ exportRange export

convertModule :: String -> Module a -> AST.Module
convertModule fileName module'@(Module _ _ modName exps _ imps decls _) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ moduleRange module'
    imps' = importCtr. convertImportDecl fileName <$> imps
    decls' = convertDeclaration fileName =<< decls
    exps' = map (convertExport fileName) . toList . wrpValue <$> exps
  uncurry AST.Module ann (nameValue modName) (imps' <> decls') exps'
  where
  importCtr (a, b, c, d) = AST.ImportDeclaration a b c d

ctrFields :: [N.Ident]
ctrFields = [N.Ident ("value" <> Text.pack (show (n :: Integer))) | n <- [0..]]
