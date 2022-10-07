-- |
-- This module implements a simple linting pass on the PureScript AST.
--
module Language.PureScript.Linter (lint, module L) where

import Prelude

import Control.Monad.Writer.Class

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad ((<=<))

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Linter.Exhaustive as L
import Language.PureScript.Linter.Imports as L
import Language.PureScript.Names
import Language.PureScript.Types
import qualified Language.PureScript.Constants.Prelude as C

-- | Lint the PureScript AST.
-- |
-- | Right now, this pass performs a shadowing check and a check for unused bindings.
lint :: forall m. (MonadWriter MultipleErrors m) => Module -> m ()
lint modl@(Module _ _ mn ds _) = do
  lintUnused modl
  censor (addHint (ErrorInModule mn)) $ mapM_ lintDeclaration ds

  where
  moduleNames :: S.Set ScopedIdent
  moduleNames = S.fromList (map ToplevelIdent (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent = getIdentName <=< declName

  lintDeclaration :: Declaration -> m ()
  lintDeclaration = tell . f
    where
    (warningsInDecl, _, _, _, _) = everythingWithScope (\_ _ -> mempty) stepE stepB (\_ _ -> mempty) stepDo

    f :: Declaration -> MultipleErrors
    f (TypeClassDeclaration _ name args _ _ decs) = addHint (ErrorInTypeClassDeclaration name) (foldMap (f' (S.fromList $ fst <$> args)) decs)
    f dec = f' S.empty dec

    f' :: S.Set Text -> Declaration -> MultipleErrors
    f' s dec@(ValueDeclaration vd) =
      addHint (ErrorInValueDeclaration (valdeclIdent vd)) (warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec)
    f' s (TypeDeclaration td@(TypeDeclarationData (ss, _) _ _)) =
      addHint (ErrorInTypeDeclaration (tydeclIdent td)) (checkTypeVars ss s (tydeclType td))
    f' s dec = warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec

    stepE :: S.Set ScopedIdent -> Expr -> MultipleErrors
    stepE s (Abs (VarBinder ss name) _) | name `inScope` s = errorMessage' ss (ShadowedName name)
    stepE s (Let _ ds' _) = foldMap go ds'
      where
      go d | Just i <- getDeclIdent d
           , inScope i s = errorMessage' (declSourceSpan d) (ShadowedName i)
           | otherwise = mempty
    stepE _ _ = mempty

    stepB :: S.Set ScopedIdent -> Binder -> MultipleErrors
    stepB s (VarBinder ss name)
      | name `inScope` s
      = errorMessage' ss (ShadowedName name)
    stepB s (NamedBinder ss name _)
      | inScope name s
      = errorMessage' ss (ShadowedName name)
    stepB _ _ = mempty

    stepDo :: S.Set ScopedIdent -> DoNotationElement -> MultipleErrors
    stepDo s (DoNotationLet ds') = foldMap go ds'
      where
      go d
        | Just i <- getDeclIdent d, i `inScope` s = errorMessage' (declSourceSpan d) (ShadowedName i)
        | otherwise = mempty
    stepDo _ _ = mempty

  checkTypeVarsInDecl :: S.Set Text -> Declaration -> MultipleErrors
  checkTypeVarsInDecl s d = let (f, _, _, _, _) = accumTypes (checkTypeVars (declSourceSpan d) s) in f d

  checkTypeVars :: SourceSpan -> S.Set Text -> SourceType -> MultipleErrors
  checkTypeVars ss set ty = everythingWithContextOnTypes set mempty mappend step ty <> snd (findUnused ty)
    where

    step :: S.Set Text -> SourceType -> (S.Set Text, MultipleErrors)
    step s (ForAll _ tv _ _ _) = bindVar s tv
    step s _ = (s, mempty)

    bindVar :: S.Set Text -> Text -> (S.Set Text, MultipleErrors)
    bindVar = bind ss ShadowedTypeVar

    findUnused :: SourceType -> (S.Set Text, MultipleErrors)
    findUnused = go set where
      -- Recursively walk the type and prune used variables from `unused`
      go :: S.Set Text -> SourceType -> (S.Set Text, MultipleErrors)
      go unused (TypeVar _ v) = (S.delete v unused, mempty)
      go unused (ForAll _ tv mbK t1 _) =
        let (nowUnused, errors)
              | Just k <- mbK = go unused k `combine` go (S.insert tv unused) t1
              | otherwise = go (S.insert tv unused) t1
            restoredUnused = if S.member tv unused then S.insert tv nowUnused else nowUnused
            combinedErrors = if S.member tv nowUnused then errors <> errorMessage' ss (UnusedTypeVar tv) else errors
        in (restoredUnused, combinedErrors)
      go unused (TypeApp _ f x) = go unused f `combine` go unused x
      go unused (KindApp _ f x) = go unused f `combine` go unused x
      go unused (ConstrainedType _ c t1) = foldl combine (unused, mempty) $ map (go unused) (constraintArgs c <> [t1])
      go unused (RCons _ _ t1 rest) = go unused t1 `combine` go unused rest
      go unused (KindedType _ t1 _) = go unused t1
      go unused (ParensInType _ t1) = go unused t1
      go unused (BinaryNoParensType _ t1 t2 t3) = go unused t1 `combine` go unused t2 `combine` go unused t3
      go unused TUnknown{} = (unused, mempty)
      go unused TypeLevelString{} = (unused, mempty)
      go unused TypeLevelInt{} = (unused, mempty)
      go unused TypeWildcard{} = (unused, mempty)
      go unused TypeConstructor{} = (unused, mempty)
      go unused TypeOp{} = (unused, mempty)
      go unused Skolem{} = (unused, mempty)
      go unused REmpty{} = (unused, mempty)

      combine ::
        (S.Set Text, MultipleErrors) ->
        (S.Set Text, MultipleErrors) ->
        (S.Set Text, MultipleErrors)
      combine (a, b) (c, d) = (S.intersection a c, b <> d)

  bind :: (Ord a) => SourceSpan -> (a -> SimpleErrorMessage) -> S.Set a -> a -> (S.Set a, MultipleErrors)
  bind ss mkError s name
    | name `S.member` s = (s, errorMessage' ss (mkError name))
    | otherwise = (S.insert name s, mempty)



lintUnused :: forall m. (MonadWriter MultipleErrors m) => Module -> m ()
lintUnused (Module modSS _ mn modDecls exports) =
  censor (addHint (ErrorInModule mn)) $ do
    topVars <- traverse lintDeclaration modDecls
    let allVars = S.unions topVars
    case exports of
      Nothing ->
        pure ()
      Just exports'
        | any thisModuleRef exports' -> pure ()
        | otherwise -> do
          let exportIds = S.fromList $ mapMaybe getValueRef exports'
              expectedUsedDecls = S.fromList (mapMaybe getDeclIdent $ filter isValueDecl modDecls) `S.difference` exportIds
              unused = (expectedUsedDecls `S.difference` allVars) `S.difference` rebindable
              newErrs = mconcat $ map unusedDeclError $ S.toList unused
          tell newErrs
          pure ()
  where
  unusedDeclError ident = errorMessage' ss $ UnusedDeclaration ident
    where
      ss = case filter ((== Just ident) . getDeclIdent) modDecls of
                  decl:_ -> declSourceSpan decl
                  _ -> modSS

  thisModuleRef :: DeclarationRef -> Bool
  thisModuleRef (ModuleRef _ mn') = mn == mn'
  thisModuleRef _ = False

  rebindable :: S.Set Ident
  rebindable = S.fromList [ Ident C.bind, Ident C.discard ]

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent = getIdentName <=< declName

  lintDeclaration :: Declaration -> m (S.Set Ident)
  lintDeclaration declToLint = do
    let (vars, errs) = goDecl declToLint
    tell errs
    pure vars
    where

    goDecl :: Declaration -> (S.Set Ident, MultipleErrors)
    goDecl (ValueDeclaration vd) =
        let allExprs = concatMap unguard $ valdeclExpression vd
            bindNewNames = S.fromList (concatMap binderNamesWithSpans $ valdeclBinders vd)
            (vars, errs) = removeAndWarn bindNewNames $ mconcat $ map go allExprs
            errs' = addHint (ErrorInValueDeclaration $ valdeclIdent vd) errs
        in
          (vars, errs')

    goDecl (TypeInstanceDeclaration _ _ _ _ _ _ _ _ (ExplicitInstance decls)) = mconcat $ map goDecl decls
    goDecl _ = mempty

    go :: Expr -> (S.Set Ident, MultipleErrors)
    go (Var _ (Qualified (BySourcePos _) v)) = (S.singleton v, mempty)
    go (Var _ _) = (S.empty, mempty)

    go (Let _ ds e) = onDecls ds (go e)

    go (Abs binder v1) =
      let newNames = S.fromList (binderNamesWithSpans binder)
      in
      removeAndWarn newNames $ go v1

    go (UnaryMinus _ v1) = go v1
    go (BinaryNoParens v0 v1 v2) = go v0 <> go v1 <> go v2
    go (Parens v1) = go v1
    go (Accessor _ v1) = go v1

    go (ObjectUpdate obj vs) = mconcat (go obj : map (go . snd) vs)
    go (ObjectUpdateNested obj vs) = go obj <> goTree vs
      where
        goTree (PathTree tree) = mconcat $ map (goNode . snd) (runAssocList tree)
        goNode (Leaf val) = go val
        goNode (Branch val) = goTree val

    go (App v1 v2) = go v1 <> go v2
    go (Unused v) = go v
    go (IfThenElse v1 v2 v3) = go v1 <> go v2 <> go v3
    go (Case vs alts) =
      let f (CaseAlternative binders gexprs) =
            let bindNewNames = S.fromList (concatMap binderNamesWithSpans binders)
                allExprs = concatMap unguard gexprs
            in
                removeAndWarn bindNewNames $ mconcat $ map go allExprs
      in
      mconcat $ map go vs ++ map f alts

    go (TypedValue _ v1 _) = go v1
    go (Do _ es) = doElts es Nothing
    go (Ado _ es v1) = doElts es (Just v1)

    go (Literal _ (ArrayLiteral es)) = mconcat $ map go es
    go (Literal _ (ObjectLiteral oo)) = mconcat $ map (go . snd) oo

    go (PositionedValue _ _ v1) = go v1

    go (Literal _ _) = mempty
    go (Op _ _) = mempty
    go (Constructor _ _) = mempty
    go (TypeClassDictionary _ _ _) = mempty
    go (DeferredDictionary _ _) = mempty
    go (DerivedInstancePlaceholder _ _) = mempty
    go AnonymousArgument = mempty
    go (Hole _) = mempty


    doElts :: [DoNotationElement] -> Maybe Expr -> (S.Set Ident, MultipleErrors)
    doElts (DoNotationValue e : rest) v = go e <> doElts rest v
    doElts (DoNotationBind binder e : rest) v =
      let bindNewNames = S.fromList (binderNamesWithSpans binder)
      in go e <> removeAndWarn bindNewNames (doElts rest v)

    doElts (DoNotationLet ds : rest) v = onDecls ds (doElts rest v)

    doElts (PositionedDoNotationElement _ _ e : rest) v = doElts (e : rest) v
    doElts [] (Just e) = go e <> (rebindable, mempty)
    doElts [] Nothing = (rebindable, mempty)

    -- (non-recursively, recursively) bound idents in decl
    declIdents :: Declaration -> (S.Set (SourceSpan, Ident), S.Set (SourceSpan, Ident))
    declIdents (ValueDecl (ss,_) ident _ _ _) = (S.empty, S.singleton (ss, ident))
    declIdents (BoundValueDeclaration _ binders _) = (S.fromList $ binderNamesWithSpans binders, S.empty)
    declIdents _ = (S.empty, S.empty)

    onDecls :: [ Declaration ] -> (S.Set Ident, MultipleErrors) -> (S.Set Ident, MultipleErrors)
    onDecls ds errs = 
      let 
        onDecl d (accErrs, accLetNamesRec) = 
            let (letNames, recNames) = declIdents d
                dErrs = underDecl d
                errs' = dErrs <> removeAndWarn letNames accErrs
            in
                (errs', accLetNamesRec <> recNames)
        (errs'', letNamesRec) = foldr onDecl (errs, S.empty) ds
      in
        removeAndWarn letNamesRec errs''

    -- let f x = e  -- check the x in e (but not the f)
    underDecl (ValueDecl _ _ _ binders gexprs) =
      let bindNewNames = S.fromList (concatMap binderNamesWithSpans binders)
          allExprs = concatMap unguard gexprs
      in
          removeAndWarn bindNewNames $ foldr1 (<>) $ map go allExprs
    -- let {x} = e  -- no binding to check inside e
    underDecl (BoundValueDeclaration _ _ expr) = go expr
    underDecl _ = (mempty, mempty)

    unguard (GuardedExpr guards expr) = map unguard' guards ++ [expr]
    unguard' (ConditionGuard ee) = ee
    unguard' (PatternGuard _ ee) = ee

    removeAndWarn :: S.Set (SourceSpan, Ident) -> (S.Set Ident, MultipleErrors) -> (S.Set Ident, MultipleErrors)
    removeAndWarn newNamesWithSpans (used, errors) =
      let newNames = S.map snd newNamesWithSpans
          filteredUsed = used `S.difference` newNames
          warnUnused = S.filter (not . Text.isPrefixOf "_" . runIdent) (newNames `S.difference` used)
          warnUnusedSpans = S.filter (\(_,ident) -> ident `elem` warnUnused) newNamesWithSpans 
          combinedErrors = if not $ S.null warnUnusedSpans then errors <> mconcat (map (\(ss,ident) -> errorMessage' ss $ UnusedName ident) $ S.toList warnUnusedSpans) else errors
      in
        (filteredUsed, combinedErrors)
