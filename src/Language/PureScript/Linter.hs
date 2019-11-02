-- |
-- This module implements a simple linting pass on the PureScript AST.
--
module Language.PureScript.Linter (lint, module L) where

import Prelude.Compat

import Control.Monad.Writer.Class

import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Linter.Exhaustive as L
import Language.PureScript.Linter.Imports as L
import Language.PureScript.Names
import Language.PureScript.Types

-- | Lint the PureScript AST.
-- |
-- | Right now, this pass only performs a shadowing check.
lint :: forall m. (MonadWriter MultipleErrors m) => Module -> m ()
lint (Module _ _ mn ds _) = censor (addHint (ErrorInModule mn)) $ mapM_ lintDeclaration ds
  where
  moduleNames :: S.Set ScopedIdent
  moduleNames = S.fromList (map ToplevelIdent (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (ValueDeclaration vd) = Just (valdeclIdent vd)
  getDeclIdent (ExternDeclaration _ ident _) = Just ident
  getDeclIdent (TypeInstanceDeclaration _ _ _ ident _ _ _ _) = Just ident
  getDeclIdent BindingGroupDeclaration{} = internalError "lint: binding groups should not be desugared yet."
  getDeclIdent _ = Nothing

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
      go unused (ForAll _ tv _ t1 _) =
        let (nowUnused, errors) = go (S.insert tv unused) t1
            restoredUnused = if S.member tv unused then S.insert tv nowUnused else nowUnused
            combinedErrors = if S.member tv nowUnused then errors <> errorMessage' ss (UnusedTypeVar tv) else errors
        in (restoredUnused, combinedErrors)
      go unused (TypeApp _ f x) = go unused f `combine` go unused x
      go unused (ConstrainedType _ c t1) = foldl combine (unused, mempty) $ map (go unused) (constraintArgs c <> [t1])
      go unused (RCons _ _ t1 rest) = go unused t1 `combine` go unused rest
      go unused (KindedType _ t1 _) = go unused t1
      go unused (ParensInType _ t1) = go unused t1
      go unused (BinaryNoParensType _ t1 t2 t3) = go unused t1 `combine` go unused t2 `combine` go unused t3
      go unused TUnknown{} = (unused, mempty)
      go unused TypeLevelString{} = (unused, mempty)
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

