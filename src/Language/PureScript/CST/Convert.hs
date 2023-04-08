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
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Text qualified as Text
import Language.PureScript.AST qualified as AST
import Language.PureScript.AST.Declarations.ChainId (mkChainId)
import Language.PureScript.AST.SourcePos qualified as Pos
import Language.PureScript.Comments qualified as C
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment qualified as Env
import Language.PureScript.Label qualified as L
import Language.PureScript.Names qualified as N
import Language.PureScript.PSString (mkString, prettyPrintStringJS)
import Language.PureScript.Types qualified as T
import Language.PureScript.CST.Positions qualified as CP
import Language.PureScript.CST.Print (printToken)
import Language.PureScript.CST.Types qualified as CT

comment :: CT.Comment a -> Maybe C.Comment
comment = \case
  CT.Comment t
    | "{-" `Text.isPrefixOf` t -> Just $ C.BlockComment $ Text.drop 2 $ Text.dropEnd 2 t
    | "--" `Text.isPrefixOf` t -> Just $ C.LineComment $ Text.drop 2 t
  _ -> Nothing

comments :: [CT.Comment a] -> [C.Comment]
comments = mapMaybe comment

sourcePos :: CT.SourcePos -> Pos.SourcePos
sourcePos (CT.SourcePos line col) = Pos.SourcePos line col

sourceSpan :: String -> CT.SourceRange -> Pos.SourceSpan
sourceSpan name (CT.SourceRange start end) = Pos.SourceSpan name (sourcePos start) (sourcePos end)

widenLeft :: CT.TokenAnn -> Pos.SourceAnn -> Pos.SourceAnn
widenLeft ann (sp, _) =
  ( Pos.widenSourceSpan (sourceSpan (Pos.spanName sp) $ CT.tokRange ann) sp
  , comments $ CT.tokLeadingComments ann
  )

sourceAnnCommented :: String -> CT.SourceToken -> CT.SourceToken -> Pos.SourceAnn
sourceAnnCommented fileName (CT.SourceToken ann1 _) (CT.SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ CT.srcStart $ CT.tokRange ann1) (sourcePos $ CT.srcEnd $ CT.tokRange ann2)
  , comments $ CT.tokLeadingComments ann1
  )

sourceAnn :: String -> CT.SourceToken -> CT.SourceToken -> Pos.SourceAnn
sourceAnn fileName (CT.SourceToken ann1 _) (CT.SourceToken ann2 _) =
  ( Pos.SourceSpan fileName (sourcePos $ CT.srcStart $ CT.tokRange ann1) (sourcePos $ CT.srcEnd $ CT.tokRange ann2)
  , []
  )

sourceName :: String -> CT.Name a -> Pos.SourceAnn
sourceName fileName a = sourceAnnCommented fileName (CT.nameTok a) (CT.nameTok a)

sourceQualName :: String -> CT.QualifiedName a -> Pos.SourceAnn
sourceQualName fileName a = sourceAnnCommented fileName (CT.qualTok a) (CT.qualTok a)

moduleName :: CT.Token -> Maybe N.ModuleName
moduleName = \case
  CT.TokLowerName as _ -> go as
  CT.TokUpperName as _ -> go as
  CT.TokSymbolName as _ -> go as
  CT.TokOperator as _ -> go as
  _ -> Nothing
  where
  go [] = Nothing
  go ns = Just $ N.ModuleName $ Text.intercalate "." ns

qualified :: CT.QualifiedName a -> N.Qualified a
qualified q = N.Qualified qb (CT.qualName q)
  where
  qb = maybe N.ByNullSourcePos N.ByModuleName $ CT.qualModule q

ident :: CT.Ident -> N.Ident
ident = N.Ident . CT.getIdent

convertType :: String -> CT.Type a -> T.SourceType
convertType fileName = go
  where
  goRow (CT.Row labels tl) b = do
    let
      rowTail = case tl of
        Just (_, ty) -> go ty
        Nothing -> T.REmpty $ sourceAnnCommented fileName b b
      rowCons (CT.Labeled a _ ty) c = do
        let ann = sourceAnnCommented fileName (CT.lblTok a) (snd $ CP.typeRange ty)
        T.RCons ann (L.Label $ CT.lblName a) (go ty) c
    case labels of
      Just (CT.Separated h t) ->
        rowCons h $ foldr (rowCons . snd) rowTail t
      Nothing ->
        rowTail

  go = \case
    CT.TypeVar _ a ->
      T.TypeVar (sourceName fileName a) . CT.getIdent $ CT.nameValue a
    CT.TypeConstructor _ a ->
      T.TypeConstructor (sourceQualName fileName a) $ qualified a
    CT.TypeWildcard _ a ->
      T.TypeWildcard (sourceAnnCommented fileName a a) T.UnnamedWildcard
    CT.TypeHole _ a ->
      T.TypeWildcard (sourceName fileName a) . T.HoleWildcard . CT.getIdent $ CT.nameValue a
    CT.TypeString _ a b ->
      T.TypeLevelString (sourceAnnCommented fileName a a) b
    CT.TypeInt _ _ a b ->
      T.TypeLevelInt (sourceAnnCommented fileName a a) b
    CT.TypeRow _ (CT.Wrapped _ row b) ->
      goRow row b
    CT.TypeRecord _ (CT.Wrapped a row b) -> do
      let
        ann = sourceAnnCommented fileName a b
        annRec = sourceAnn fileName a a
      T.TypeApp ann (Env.tyRecord $> annRec) $ goRow row b
    CT.TypeForall _ kw bindings _ ty -> do
      let
        mkForAll a b t = do
          let ann' = widenLeft (CT.tokAnn $ CT.nameTok a) $ T.getAnnForType t
          T.ForAll ann' (CT.getIdent $ CT.nameValue a) b t Nothing
        k (CT.TypeVarKinded (CT.Wrapped _ (CT.Labeled a _ b) _)) = mkForAll a (Just (go b))
        k (CT.TypeVarName a) = mkForAll a Nothing
        ty' = foldr k (go ty) bindings
        ann = widenLeft (CT.tokAnn kw) $ T.getAnnForType ty'
      T.setAnnForType ann ty'
    CT.TypeKinded _ ty _ kd -> do
      let
        ty' = go ty
        kd' = go kd
        ann = Pos.widenSourceAnn (T.getAnnForType ty') (T.getAnnForType kd')
      T.KindedType ann ty' kd'
    CT.TypeApp _ a b -> do
      let
        a' = go a
        b' = go b
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann a' b'
    ty@(CT.TypeOp _ _ _ _) -> do
      let
        reassoc op b' a = do
          let
            a'  = go a
            op' = T.TypeOp (sourceQualName fileName op) $ qualified op
            ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
          T.BinaryNoParensType ann op' (go a) b'
        loop k = \case
          CT.TypeOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      loop go ty
    CT.TypeOpName _ op -> do
      let rng = CP.qualRange op
      T.TypeOp (uncurry (sourceAnnCommented fileName) rng) (qualified op)
    CT.TypeArr _ a arr b -> do
      let
        a' = go a
        b' = go b
        arr' = Env.tyFunction $> sourceAnnCommented fileName arr arr
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann (T.TypeApp ann arr' a') b'
    CT.TypeArrName _ a ->
      Env.tyFunction $> sourceAnnCommented fileName a a
    CT.TypeConstrained _ a _ b -> do
      let
        a' = convertConstraint fileName a
        b' = go b
        ann = Pos.widenSourceAnn (T.constraintAnn a') (T.getAnnForType b')
      T.ConstrainedType ann a' b'
    CT.TypeParens _ (CT.Wrapped a ty b) ->
      T.ParensInType (sourceAnnCommented fileName a b) $ go ty
    ty@(CT.TypeUnaryRow _ _ a) -> do
      let
        a' = go a
        rng = CP.typeRange ty
        ann = uncurry (sourceAnnCommented fileName) rng
      T.setAnnForType ann $ Env.kindRow a'

convertConstraint :: String -> CT.Constraint a -> T.SourceConstraint
convertConstraint fileName = go
  where
  go = \case
    cst@(CT.Constraint _ name args) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.constraintRange cst
      T.Constraint ann (qualified name) [] (convertType fileName <$> args) Nothing
    CT.ConstraintParens _ (CT.Wrapped _ c _) -> go c

convertGuarded :: String -> CT.Guarded a -> [AST.GuardedExpr]
convertGuarded fileName = \case
  CT.Unconditional _ x -> [AST.GuardedExpr [] (convertWhere fileName x)]
  CT.Guarded gs -> (\(CT.GuardedExpr _ ps _ x) -> AST.GuardedExpr (p <$> toList ps) (convertWhere fileName x)) <$> NE.toList gs
  where
  go = convertExpr fileName
  p (CT.PatternGuard Nothing x) = AST.ConditionGuard (go x)
  p (CT.PatternGuard (Just (b, _)) x) = AST.PatternGuard (convertBinder fileName b) (go x)

convertWhere :: String -> CT.Where a -> AST.Expr
convertWhere fileName = \case
  CT.Where expr Nothing -> convertExpr fileName expr
  CT.Where expr (Just (_, bs)) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
    uncurry AST.PositionedValue ann . AST.Let AST.FromWhere (convertLetBinding fileName <$> NE.toList bs) $ convertExpr fileName expr

convertLetBinding :: String -> CT.LetBinding a -> AST.Declaration
convertLetBinding fileName = \case
  CT.LetBindingSignature _ lbl ->
    convertSignature fileName lbl
  binding@(CT.LetBindingName _ fields) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ CP.letBindingRange binding
    convertValueBindingFields fileName ann fields
  binding@(CT.LetBindingPattern _ a _ b) -> do
    let ann = uncurry (sourceAnnCommented fileName) $ CP.letBindingRange binding
    AST.BoundValueDeclaration ann (convertBinder fileName a) (convertWhere fileName b)

convertExpr :: forall a. String -> CT.Expr a -> AST.Expr
convertExpr fileName = go
  where
  positioned =
    uncurry AST.PositionedValue

  goDoStatement = \case
    stmt@(CT.DoLet _ as) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationLet $ convertLetBinding fileName <$> NE.toList as
    stmt@(CT.DoDiscard a) -> do
      let ann = uncurry (sourceAnn fileName) $ CP.doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationValue $ go a
    stmt@(CT.DoBind a _ b) -> do
      let
        ann = uncurry (sourceAnn fileName) $ CP.doStatementRange stmt
        a' = convertBinder fileName a
        b' = go b
      uncurry AST.PositionedDoNotationElement ann $ AST.DoNotationBind a' b'

  go = \case
    CT.ExprHole _ a ->
      positioned (sourceName fileName a) . AST.Hole . CT.getIdent $ CT.nameValue a
    CT.ExprSection _ a ->
      positioned (sourceAnnCommented fileName a a) AST.AnonymousArgument
    CT.ExprIdent _ a -> do
      let ann = sourceQualName fileName a
      positioned ann . AST.Var (fst ann) . qualified $ fmap ident a
    CT.ExprConstructor _ a -> do
      let ann = sourceQualName fileName a
      positioned ann . AST.Constructor (fst ann) $ qualified a
    CT.ExprBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.BooleanLiteral b
    CT.ExprChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.CharLiteral b
    CT.ExprString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) . AST.StringLiteral $ b
    CT.ExprNumber _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.NumericLiteral b
    CT.ExprArray _ (CT.Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (CT.Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ArrayLiteral vals
    CT.ExprRecord z (CT.Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          CT.RecordPun f -> (mkString . CT.getIdent $ CT.nameValue f, go . CT.ExprIdent z $ CT.QualifiedName (CT.nameTok f) Nothing (CT.nameValue f))
          CT.RecordField f _ v -> (CT.lblName f, go v)
        vals = case bs of
          Just (CT.Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ObjectLiteral vals
    CT.ExprParens _ (CT.Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.Parens $ go b
    expr@(CT.ExprTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . CP.toSourceRange $ CP.exprRange expr, [])
      positioned ann $ AST.TypedValue True a' b'
    expr@(CT.ExprInfix _ a (CT.Wrapped _ b _) c) -> do
      let ann = (sourceSpan fileName . CP.toSourceRange $ CP.exprRange expr, [])
      positioned ann $ AST.BinaryNoParens (go b) (go a) (go c)
    expr@(CT.ExprOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ CP.exprRange expr
        reassoc op b a = do
          let op' = AST.Op (sourceSpan fileName . CP.toSourceRange $ CP.qualRange op) $ qualified op
          AST.BinaryNoParens op' (go a) b
        loop k = \case
          CT.ExprOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      positioned ann $ loop go expr
    CT.ExprOpName _ op -> do
      let
        rng = CP.qualRange op
        op' = AST.Op (sourceSpan fileName $ CP.toSourceRange rng) $ qualified op
      positioned (uncurry (sourceAnnCommented fileName) rng) op'
    expr@(CT.ExprNegate _ _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann . AST.UnaryMinus (fst ann) $ go b
    expr@(CT.ExprRecordAccessor _ (CT.RecordAccessor a _ (CT.Separated h t))) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
        field x f = AST.Accessor (CT.lblName f) x
      positioned ann $ foldl' (\x (_, f) -> field x f) (field (go a) h) t
    expr@(CT.ExprRecordUpdate _ a b) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
        k (CT.RecordUpdateLeaf f _ x) = (CT.lblName f, AST.Leaf $ go x)
        k (CT.RecordUpdateBranch f xs) = (CT.lblName f, AST.Branch $ toTree xs)
        toTree (CT.Wrapped _ xs _) = AST.PathTree . AST.AssocList . map k $ toList xs
      positioned ann . AST.ObjectUpdateNested (go a) $ toTree b
    expr@(CT.ExprApp _ a b) -> do
      let ann = uncurry (sourceAnn fileName) $ CP.exprRange expr
      positioned ann $ AST.App (go a) (go b)
    expr@(CT.ExprLambda _ (CT.Lambda _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann
        . AST.Abs (convertBinder fileName (NE.head as))
        . foldr (AST.Abs . convertBinder fileName) (go b)
        $ NE.tail as
    expr@(CT.ExprIf _ (CT.IfThenElse _ a _ b _ c)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann $ AST.IfThenElse (go a) (go b) (go c)
    expr@(CT.ExprCase _ (CT.CaseOf _ as _ bs)) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
        as' = go <$> toList as
        bs' = uncurry AST.CaseAlternative . bimap (map (convertBinder fileName) . toList) (convertGuarded fileName) <$> NE.toList bs
      positioned ann $ AST.Case as' bs'
    expr@(CT.ExprLet _ (CT.LetIn _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann . AST.Let AST.FromLet (convertLetBinding fileName <$> NE.toList as) $ go b
    -- expr@(ExprWhere _ (Where a _ bs)) -> do
    --   let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
    --   positioned ann . AST.Let AST.FromWhere (goLetBinding <$> bs) $ go a
    expr@(CT.ExprDo _ (CT.DoBlock kw stmts)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann . AST.Do (moduleName $ CT.tokValue kw) $ goDoStatement <$> NE.toList stmts
    expr@(CT.ExprAdo _ (CT.AdoBlock kw stms _ a)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.exprRange expr
      positioned ann . AST.Ado (moduleName $ CT.tokValue kw) (goDoStatement <$> stms) $ go a

convertBinder :: String -> CT.Binder a -> AST.Binder
convertBinder fileName = go
  where
  positioned =
    uncurry AST.PositionedBinder

  go = \case
    CT.BinderWildcard _ a ->
      positioned (sourceAnnCommented fileName a a) AST.NullBinder
    CT.BinderVar _ a -> do
      let ann = sourceName fileName a
      positioned ann . AST.VarBinder (fst ann) . ident $ CT.nameValue a
    binder@(CT.BinderNamed _ a _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.binderRange binder
      positioned ann . AST.NamedBinder (fst ann) (ident $ CT.nameValue a) $ go b
    binder@(CT.BinderConstructor _ a bs) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ CP.binderRange binder
      positioned ann . AST.ConstructorBinder (fst ann) (qualified a) $ go <$> bs
    CT.BinderBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.BooleanLiteral b
    CT.BinderChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.CharLiteral b
    CT.BinderString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) . AST.StringLiteral $ b
    CT.BinderNumber _ n a b -> do
      let
        ann = sourceAnnCommented fileName a a
        b'
          | isJust n = bimap negate negate b
          | otherwise = b
      positioned ann . AST.LiteralBinder (fst ann) $ AST.NumericLiteral b'
    CT.BinderArray _ (CT.Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (CT.Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ArrayLiteral vals
    CT.BinderRecord z (CT.Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          CT.RecordPun f -> (mkString . CT.getIdent $ CT.nameValue f, go $ CT.BinderVar z f)
          CT.RecordField f _ v -> (CT.lblName f, go v)
        vals = case bs of
          Just (CT.Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ObjectLiteral vals
    CT.BinderParens _ (CT.Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.ParensInBinder $ go b
    binder@(CT.BinderTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . CP.toSourceRange $ CP.binderRange binder, [])
      positioned ann $ AST.TypedBinder b' a'
    binder@(CT.BinderOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ CP.binderRange binder
        reassoc op b a = do
          let op' = AST.OpBinder (sourceSpan fileName . CP.toSourceRange $ CP.qualRange op) $ qualified op
          AST.BinaryNoParensBinder op' (go a) b
        loop k = \case
          CT.BinderOp _ a op b -> loop (reassoc op (k b)) a
          binder' -> k binder'
      positioned ann $ loop go binder

convertDeclaration :: String -> CT.Declaration a -> [AST.Declaration]
convertDeclaration fileName decl = case decl of
  CT.DeclData _ (CT.DataHead _ a vars) bd -> do
    let
      ctrs :: CT.SourceToken -> CT.DataCtor a -> [(CT.SourceToken, CT.DataCtor a)] -> [AST.DataConstructorDeclaration]
      ctrs st (CT.DataCtor _ name fields) tl
        = AST.DataConstructorDeclaration (sourceAnnCommented fileName st (CT.nameTok name)) (CT.nameValue name) (zip ctrFields $ convertType fileName <$> fields)
        : (case tl of
            [] -> []
            (st', ctor) : tl' -> ctrs st' ctor tl'
          )
    pure $ AST.DataDeclaration ann Env.Data (CT.nameValue a) (goTypeVar <$> vars) (maybe [] (\(st, CT.Separated hd tl) -> ctrs st hd tl) bd)
  CT.DeclType _ (CT.DataHead _ a vars) _ bd ->
    pure $ AST.TypeSynonymDeclaration ann
      (CT.nameValue a)
      (goTypeVar <$> vars)
      (convertType fileName bd)
  CT.DeclNewtype _ (CT.DataHead _ a vars) st x ys -> do
    let ctrs = [AST.DataConstructorDeclaration (sourceAnnCommented fileName st (snd $ CP.declRange decl)) (CT.nameValue x) [(head ctrFields, convertType fileName ys)]]
    pure $ AST.DataDeclaration ann Env.Newtype (CT.nameValue a) (goTypeVar <$> vars) ctrs
  CT.DeclClass _ (CT.ClassHead _ sup name vars fdeps) bd -> do
    let
      goTyVar (CT.TypeVarKinded (CT.Wrapped _ (CT.Labeled a _ _) _)) = CT.nameValue a
      goTyVar (CT.TypeVarName a) = CT.nameValue a
      vars' = zip (toList $ goTyVar <$> vars) [0..]
      goName = fromJust . flip lookup vars' . CT.nameValue
      goFundep (CT.FundepDetermined _ bs) = Env.FunctionalDependency [] (goName <$> NE.toList bs)
      goFundep (CT.FundepDetermines as _ bs) = Env.FunctionalDependency (goName <$> NE.toList as) (goName <$> NE.toList bs)
      goSig (CT.Labeled n _ ty) = do
        let
          ty' = convertType fileName ty
          ann' = widenLeft (CT.tokAnn $ CT.nameTok n) $ T.getAnnForType ty'
        AST.TypeDeclaration $ AST.TypeDeclarationData ann' (ident $ CT.nameValue n) ty'
    pure $ AST.TypeClassDeclaration ann
      (CT.nameValue name)
      (goTypeVar <$> vars)
      (convertConstraint fileName <$> maybe [] (toList . fst) sup)
      (goFundep <$> maybe [] (toList . snd) fdeps)
      (goSig <$> maybe [] (NE.toList . snd) bd)
  CT.DeclInstanceChain _ insts -> do
    let
      chainId = mkChainId fileName $ startSourcePos $ CT.instKeyword $ CT.instHead $ CT.sepHead insts
      goInst ix inst@(CT.Instance (CT.InstanceHead _ nameSep ctrs cls args) bd) = do
        let ann' = uncurry (sourceAnnCommented fileName) $ CP.instanceRange inst
            clsAnn = findInstanceAnn cls args
        AST.TypeInstanceDeclaration ann' clsAnn chainId ix
          (mkPartialInstanceName nameSep cls args)
          (convertConstraint fileName <$> maybe [] (toList . fst) ctrs)
          (qualified cls)
          (convertType fileName <$> args)
          (AST.ExplicitInstance $ goInstanceBinding <$> maybe [] (NE.toList . snd) bd)
    uncurry goInst <$> zip [0..] (toList insts)
  CT.DeclDerive _ _ new (CT.InstanceHead kw nameSep ctrs cls args) -> do
    let
      chainId = mkChainId fileName $ startSourcePos kw
      name' = mkPartialInstanceName nameSep cls args
      instTy
        | isJust new = AST.NewtypeInstance
        | otherwise = AST.DerivedInstance
      clsAnn = findInstanceAnn cls args
    pure $ AST.TypeInstanceDeclaration ann clsAnn chainId 0 name'
      (convertConstraint fileName <$> maybe [] (toList . fst) ctrs)
      (qualified cls)
      (convertType fileName <$> args)
      instTy
  CT.DeclKindSignature _ kw (CT.Labeled name _ ty) -> do
    let
      kindFor = case CT.tokValue kw of
        CT.TokLowerName [] "data" -> AST.DataSig
        CT.TokLowerName [] "newtype" -> AST.NewtypeSig
        CT.TokLowerName [] "type" -> AST.TypeSynonymSig
        CT.TokLowerName [] "class" -> AST.ClassSig
        tok -> internalError $ "Invalid kind signature keyword " <> Text.unpack (printToken tok)
    pure . AST.KindDeclaration ann kindFor (CT.nameValue name) $ convertType fileName ty
  CT.DeclSignature _ lbl ->
    pure $ convertSignature fileName lbl
  CT.DeclValue _ fields ->
    pure $ convertValueBindingFields fileName ann fields
  CT.DeclFixity _ (CT.FixityFields (_, kw) (_, prec) fxop) -> do
    let
      assoc =  case kw of
        CT.Infix  -> AST.Infix
        CT.Infixr -> AST.Infixr
        CT.Infixl -> AST.Infixl
      fixity = AST.Fixity assoc prec
    pure $ AST.FixityDeclaration ann $ case fxop of
      CT.FixityValue name _ op -> do
        Left $ AST.ValueFixity fixity (first ident <$> qualified name) (CT.nameValue op)
      CT.FixityType _ name _ op ->
        Right $ AST.TypeFixity fixity (qualified name) (CT.nameValue op)
  CT.DeclForeign _ _ _ frn ->
    pure $ case frn of
      CT.ForeignValue (CT.Labeled a _ b) ->
        AST.ExternDeclaration ann (ident $ CT.nameValue a) $ convertType fileName b
      CT.ForeignData _ (CT.Labeled a _ b) ->
        AST.ExternDataDeclaration ann (CT.nameValue a) $ convertType fileName b
      CT.ForeignKind _ a ->
        AST.DataDeclaration ann Env.Data (CT.nameValue a) [] []
  CT.DeclRole _ _ _ name roles ->
    pure $ AST.RoleDeclaration $
      AST.RoleDeclarationData ann (CT.nameValue name) (CT.roleValue <$> NE.toList roles)
  where
  ann =
    uncurry (sourceAnnCommented fileName) $ CP.declRange decl

  startSourcePos :: CT.SourceToken -> Pos.SourcePos
  startSourcePos = sourcePos . CT.srcStart . CT.tokRange . CT.tokAnn

  mkPartialInstanceName :: Maybe (CT.Name CT.Ident, CT.SourceToken) -> CT.QualifiedName (N.ProperName 'N.ClassName) -> [CT.Type a] -> Either Text.Text N.Ident
  mkPartialInstanceName nameSep cls args =
    maybe (Left genName) (Right . ident . CT.nameValue . fst) nameSep
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
        $ CT.qualName cls

      typeArgs :: Text.Text
      typeArgs = foldMap argName args

      argName :: CT.Type a -> Text.Text
      argName = \case
        -- These are only useful to disambiguate between overlapping instances
        -- but they’re disallowed outside of instance chains. Since we’re
        -- avoiding name collisions with unique identifiers anyway,
        -- we don't need to render this constructor.
        CT.TypeVar{} -> ""
        CT.TypeConstructor _ qn -> N.runProperName $ CT.qualName qn
        CT.TypeOpName _ qn -> N.runOpName $ CT.qualName qn
        CT.TypeString _ _ ps -> prettyPrintStringJS ps
        CT.TypeInt _ _ _ nt -> Text.pack $ show nt

        -- Typed holes are disallowed in instance heads
        CT.TypeHole{} -> ""
        CT.TypeParens _ t -> argName $ CT.wrpValue t
        CT.TypeKinded _ t1 _ t2 -> argName t1 <> argName t2
        CT.TypeRecord _ _ -> "Record"
        CT.TypeRow _ _ -> "Row"
        CT.TypeArrName _ _ -> "Function"
        CT.TypeWildcard{} -> "_"

        -- Polytypes are disallowed in instance heads
        CT.TypeForall{} -> ""
        CT.TypeApp _ t1 t2 -> argName t1 <> argName t2
        CT.TypeOp _ t1 op t2 ->
          argName t1 <> N.runOpName (CT.qualName op) <> argName t2
        CT.TypeArr _ t1 _ t2 -> argName t1 <> "Function" <> argName t2
        CT.TypeConstrained{} -> ""
        CT.TypeUnaryRow{} -> "Row"

  goTypeVar = \case
    CT.TypeVarKinded (CT.Wrapped _ (CT.Labeled x _ y) _) -> (CT.getIdent $ CT.nameValue x, Just $ convertType fileName y)
    CT.TypeVarName x -> (CT.getIdent $ CT.nameValue x, Nothing)

  goInstanceBinding = \case
    CT.InstanceBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(CT.InstanceBindingName _ fields) -> do
      let ann' = uncurry (sourceAnnCommented fileName) $ CP.instanceBindingRange binding
      convertValueBindingFields fileName ann' fields

  findInstanceAnn cls args = uncurry (sourceAnnCommented fileName) $
    if null args then
      CP.qualRange cls
    else
      (fst $ CP.qualRange cls, snd $ CP.typeRange $ last args)

convertSignature :: String -> CT.Labeled (CT.Name CT.Ident) (CT.Type a) -> AST.Declaration
convertSignature fileName (CT.Labeled a _ b) = do
  let
    b' = convertType fileName b
    ann = widenLeft (CT.tokAnn $ CT.nameTok a) $ T.getAnnForType b'
  AST.TypeDeclaration $ AST.TypeDeclarationData ann (ident $ CT.nameValue a) b'

convertValueBindingFields :: String -> Pos.SourceAnn -> CT.ValueBindingFields a -> AST.Declaration
convertValueBindingFields fileName ann (CT.ValueBindingFields a bs c) = do
  let
    bs' = convertBinder fileName <$> bs
    cs' = convertGuarded fileName c
  AST.ValueDeclaration $ AST.ValueDeclarationData ann (ident $ CT.nameValue a) Env.Public bs' cs'

convertImportDecl
  :: String
  -> CT.ImportDecl a
  -> (Pos.SourceAnn, N.ModuleName, AST.ImportDeclarationType, Maybe N.ModuleName)
convertImportDecl fileName decl@(CT.ImportDecl _ _ modName mbNames mbQual) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ CP.importDeclRange decl
    importTy = case mbNames of
      Nothing -> AST.Implicit
      Just (hiding, CT.Wrapped _ imps _) -> do
        let imps' = convertImport fileName <$> toList imps
        if isJust hiding
          then AST.Hiding imps'
          else AST.Explicit imps'
  (ann, CT.nameValue modName, importTy, CT.nameValue . snd <$> mbQual)

convertImport :: String -> CT.Import a -> AST.DeclarationRef
convertImport fileName imp = case imp of
  CT.ImportValue _ a ->
    AST.ValueRef ann . ident $ CT.nameValue a
  CT.ImportOp _ a ->
    AST.ValueOpRef ann $ CT.nameValue a
  CT.ImportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (CT.DataAll _ _) -> Nothing
        Just (CT.DataEnumerated _ (CT.Wrapped _ Nothing _)) -> Just []
        Just (CT.DataEnumerated _ (CT.Wrapped _ (Just idents) _)) ->
          Just . map CT.nameValue $ toList idents
    AST.TypeRef ann (CT.nameValue a) ctrs
  CT.ImportTypeOp _ _ a ->
    AST.TypeOpRef ann $ CT.nameValue a
  CT.ImportClass _ _ a ->
    AST.TypeClassRef ann $ CT.nameValue a
  where
  ann = sourceSpan fileName . CP.toSourceRange $ CP.importRange imp

convertExport :: String -> CT.Export a -> AST.DeclarationRef
convertExport fileName export = case export of
  CT.ExportValue _ a ->
    AST.ValueRef ann . ident $ CT.nameValue a
  CT.ExportOp _ a ->
    AST.ValueOpRef ann $ CT.nameValue a
  CT.ExportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (CT.DataAll _ _) -> Nothing
        Just (CT.DataEnumerated _ (CT.Wrapped _ Nothing _)) -> Just []
        Just (CT.DataEnumerated _ (CT.Wrapped _ (Just idents) _)) ->
          Just . map CT.nameValue $ toList idents
    AST.TypeRef ann (CT.nameValue a) ctrs
  CT.ExportTypeOp _ _ a ->
    AST.TypeOpRef ann $ CT.nameValue a
  CT.ExportClass _ _ a ->
    AST.TypeClassRef ann $ CT.nameValue a
  CT.ExportModule _ _ a ->
    AST.ModuleRef ann (CT.nameValue a)
  where
  ann = sourceSpan fileName . CP.toSourceRange $ CP.exportRange export

convertModule :: String -> CT.Module a -> AST.Module
convertModule fileName module'@(CT.Module _ _ modName exps _ imps decls _) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ CP.moduleRange module'
    imps' = importCtr. convertImportDecl fileName <$> imps
    decls' = convertDeclaration fileName =<< decls
    exps' = map (convertExport fileName) . toList . CT.wrpValue <$> exps
  uncurry AST.Module ann (CT.nameValue modName) (imps' <> decls') exps'
  where
  importCtr (a, b, c, d) = AST.ImportDeclaration a b c d

ctrFields :: [N.Ident]
ctrFields = [N.Ident ("value" <> Text.pack (show (n :: Integer))) | n <- [0..]]
