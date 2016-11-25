-- |
-- This module implements a simple linting pass on the PureScript AST.
--
module Language.PureScript.Linter (lint, module L) where

import Prelude.Compat

import Control.Monad.Writer.Class

import Data.List (nub, (\\))
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Set as S

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
  moduleNames = S.fromList (nub (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _) = Just ident
  getDeclIdent (ExternDeclaration ident _) = Just ident
  getDeclIdent (TypeInstanceDeclaration ident _ _ _ _) = Just ident
  getDeclIdent (BindingGroupDeclaration _) = internalError "lint: binding groups should not be desugared yet."
  getDeclIdent _ = Nothing

  lintDeclaration :: Declaration -> m ()
  lintDeclaration = tell . f
    where
    (warningsInDecl, _, _, _, _) = everythingWithScope (\_ _ -> mempty) stepE stepB (\_ _ -> mempty) stepDo

    f :: Declaration -> MultipleErrors
    f (PositionedDeclaration pos _ dec) = addHint (PositionedError pos) (f dec)
    f (TypeClassDeclaration name args _ _ decs) = addHint (ErrorInTypeClassDeclaration name) (foldMap (f' (S.fromList $ fst <$> args)) decs)
    f dec = f' S.empty dec

    f' :: S.Set String -> Declaration -> MultipleErrors
    f' s (PositionedDeclaration pos _ dec) = addHint (PositionedError pos) (f' s dec)
    f' s dec@(ValueDeclaration name _ _ _) = addHint (ErrorInValueDeclaration name) (warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec)
    f' s (TypeDeclaration name ty) = addHint (ErrorInTypeDeclaration name) (checkTypeVars s ty)
    f' s dec = warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec

    stepE :: S.Set Ident -> Expr -> MultipleErrors
    stepE s (Abs (Left name) _) | name `S.member` s = errorMessage (ShadowedName name)
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

  checkTypeVarsInDecl :: S.Set String -> Declaration -> MultipleErrors
  checkTypeVarsInDecl s d = let (f, _, _, _, _) = accumTypes (checkTypeVars s) in f d

  checkTypeVars :: S.Set String -> Type -> MultipleErrors
  checkTypeVars set ty = everythingWithContextOnTypes set mempty mappend step ty <> findUnused ty
    where
    step :: S.Set String -> Type -> (S.Set String, MultipleErrors)
    step s (ForAll tv _ _) = bindVar s tv
    step s _ = (s, mempty)
    bindVar :: S.Set String -> String -> (S.Set String, MultipleErrors)
    bindVar = bind ShadowedTypeVar
    findUnused :: Type -> MultipleErrors
    findUnused ty' =
      let used = usedTypeVariables ty'
          declared = everythingOnTypes (++) go ty'
          unused = nub declared \\ nub used
      in foldl (<>) mempty $ map (errorMessage . UnusedTypeVar) unused
      where
      go :: Type -> [String]
      go (ForAll tv _ _) = [tv]
      go _ = []

  bind :: (Ord a) => (a -> SimpleErrorMessage) -> S.Set a -> a -> (S.Set a, MultipleErrors)
  bind mkError s name | name `S.member` s = (s, errorMessage (mkError name))
                      | otherwise = (S.insert name s, mempty)
