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
lint :: forall m a b. (MonadWriter MultipleErrors m) => Module a b -> m ()
lint (Module _ _ mn ds _) = censor (addHint (ErrorInModule mn)) $ mapM_ lintDeclaration ds
  where
  moduleNames :: S.Set Ident
  moduleNames = S.fromList (nub (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration a b -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration _ ident _ _ _) = Just ident
  getDeclIdent (ExternDeclaration _ ident _) = Just ident
  getDeclIdent (TypeInstanceDeclaration _ ident _ _ _ _) = Just ident
  getDeclIdent (BindingGroupDeclaration _ _) = internalError "lint: binding groups should not be desugared yet."
  getDeclIdent _ = Nothing

  lintDeclaration :: Declaration a b -> m ()
  lintDeclaration = tell . f
    where
    (warningsInDecl, _, _, _, _) = everythingWithScope (\_ _ -> mempty) stepE stepB (\_ _ -> mempty) stepDo

    f :: Declaration a b -> MultipleErrors
    f (PositionedDeclaration _ pos _ dec) = addHint (PositionedError pos) (f dec)
    f (TypeClassDeclaration _ name args _ _ decs) = addHint (ErrorInTypeClassDeclaration name) (foldMap (f' (S.fromList $ fst <$> args)) decs)
    f dec = f' S.empty dec

    f' :: S.Set Text -> Declaration a b -> MultipleErrors
    f' s (PositionedDeclaration _ pos _ dec) = addHint (PositionedError pos) (f' s dec)
    f' s dec@(ValueDeclaration _ name _ _ _) = addHint (ErrorInValueDeclaration name) (warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec)
    f' s (TypeDeclaration _ name ty) = addHint (ErrorInTypeDeclaration name) (checkTypeVars s ty)
    f' s dec = warningsInDecl moduleNames dec <> checkTypeVarsInDecl s dec

    stepE :: S.Set Ident -> Expr a b -> MultipleErrors
    stepE s (Abs _ (Left name) _) | name `S.member` s = errorMessage (ShadowedName name)
    stepE s (Let _ ds' _) = foldMap go ds'
      where
      go d | Just i <- getDeclIdent d
           , i `S.member` s = errorMessage (ShadowedName i)
           | otherwise = mempty
    stepE _ _ = mempty

    stepB :: S.Set Ident -> Binder a b -> MultipleErrors
    stepB s (VarBinder _ name) | name `S.member` s = errorMessage (ShadowedName name)
    stepB s (NamedBinder _ name _) | name `S.member` s = errorMessage (ShadowedName name)
    stepB _ _ = mempty

    stepDo :: S.Set Ident -> DoNotationElement a b -> MultipleErrors
    stepDo s (DoNotationLet _ ds') = foldMap go ds'
      where
      go d | Just i <- getDeclIdent d
           , i `S.member` s = errorMessage (ShadowedName i)
           | otherwise = mempty
    stepDo _ _ = mempty

  checkTypeVarsInDecl :: S.Set Text -> Declaration a b -> MultipleErrors
  checkTypeVarsInDecl s d = let (f, _, _, _, _) = accumTypes (checkTypeVars s) in f d

  checkTypeVars :: S.Set Text -> Type a -> MultipleErrors
  checkTypeVars set ty = everythingWithContextOnTypes set mempty mappend step ty <> findUnused ty
    where
    step :: S.Set Text -> Type a -> (S.Set Text, MultipleErrors)
    step s (ForAll _ tv _ _) = bindVar s tv
    step s _ = (s, mempty)
    bindVar :: S.Set Text -> Text -> (S.Set Text, MultipleErrors)
    bindVar = bind ShadowedTypeVar
    findUnused :: Type a -> MultipleErrors
    findUnused ty' =
      let used = usedTypeVariables ty'
          declared = everythingOnTypes (++) go ty'
          unused = nub declared \\ nub used
      in foldl (<>) mempty $ map (errorMessage . UnusedTypeVar) unused
      where
      go :: Type a -> [Text]
      go (ForAll _ tv _ _) = [tv]
      go _ = []

  bind :: Ord x => (x -> SimpleErrorMessage) -> S.Set x -> x -> (S.Set x, MultipleErrors)
  bind mkError s name | name `S.member` s = (s, errorMessage (mkError name))
                      | otherwise = (S.insert name s, mempty)
