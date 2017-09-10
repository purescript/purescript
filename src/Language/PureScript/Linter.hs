-- |
-- This module implements a simple linting pass on the PureScript AST.
--
module Language.PureScript.Linter (lint, module L) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad.Writer.Class

import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.Monoid
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
  moduleNames :: S.Set Ident
  moduleNames = S.fromList (ordNub (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (ValueDeclaration vd) = Just (valdeclIdent vd)
  getDeclIdent (ExternDeclaration _ ident _) = Just ident
  getDeclIdent (TypeInstanceDeclaration _ ident _ _ _ _) = Just ident
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
    f' s dec@(ValueDeclaration vd) = addHint (ErrorInValueDeclaration (valdeclIdent vd)) (warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec)
    f' s (TypeDeclaration td) = addHint (ErrorInTypeDeclaration (tydeclIdent td)) (checkTypeVars s (tydeclType td))
    f' s dec = warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec

    stepE :: S.Set Ident -> Expr -> MultipleErrors
    stepE s (Abs (VarBinder name) _) | name `S.member` s = errorMessage (ShadowedName name)
    stepE s (Let ds' _) = foldMap go ds'
      where
      go d | Just i <- getDeclIdent d
           , i `S.member` s = errorMessage (ShadowedName i)
           | otherwise = mempty
    stepE _ _ = mempty

    stepB :: S.Set Ident -> Binder -> MultipleErrors
    stepB s (VarBinder name) | name `S.member` s = errorMessage (ShadowedName name)
    stepB s (NamedBinder name _) | name `S.member` s = errorMessage (ShadowedName name)
    stepB _ _ = mempty

    stepDo :: S.Set Ident -> DoNotationElement -> MultipleErrors
    stepDo s (DoNotationLet ds') = foldMap go ds'
      where
      go d | Just i <- getDeclIdent d
           , i `S.member` s = errorMessage (ShadowedName i)
           | otherwise = mempty
    stepDo _ _ = mempty

  checkTypeVarsInDecl :: S.Set Text -> Declaration -> MultipleErrors
  checkTypeVarsInDecl s d = let (f, _, _, _, _) = accumTypes (checkTypeVars s) in f d

  checkTypeVars :: S.Set Text -> Type -> MultipleErrors
  checkTypeVars set ty = everythingWithContextOnTypes set mempty mappend step ty <> findUnused ty
    where
    step :: S.Set Text -> Type -> (S.Set Text, MultipleErrors)
    step s (ForAll tv _ _) = bindVar s tv
    step s _ = (s, mempty)
    bindVar :: S.Set Text -> Text -> (S.Set Text, MultipleErrors)
    bindVar = bind ShadowedTypeVar
    findUnused :: Type -> MultipleErrors
    findUnused ty' =
      let used = usedTypeVariables ty'
          declared = everythingOnTypes (++) go ty'
          unused = ordNub declared \\ ordNub used
      in foldl (<>) mempty $ map (errorMessage . UnusedTypeVar) unused
      where
      go :: Type -> [Text]
      go (ForAll tv _ _) = [tv]
      go _ = []

  bind :: (Ord a) => (a -> SimpleErrorMessage) -> S.Set a -> a -> (S.Set a, MultipleErrors)
  bind mkError s name | name `S.member` s = (s, errorMessage (mkError name))
                      | otherwise = (S.insert name s, mempty)
