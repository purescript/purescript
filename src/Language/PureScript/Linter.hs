-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Linter
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | This module implements a simple linting pass on the PureScript AST.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Linter (lint, module L) where

import Prelude ()
import Prelude.Compat

import Data.List (nub, (\\))
import Data.Maybe (mapMaybe)
import Data.Monoid

import qualified Data.Set as S

import Control.Monad.Writer.Class

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Types
import Language.PureScript.Linter.Exhaustive as L
import Language.PureScript.Linter.Imports as L

-- | Lint the PureScript AST.
-- |
-- | Right now, this pass only performs a shadowing check.
lint :: forall m. (Applicative m, MonadWriter MultipleErrors m) => Module -> m ()
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
    (warningsInDecl, _, _, _, _) = everythingWithScope stepD stepE stepB (\_ _ -> mempty) stepDo

    f :: Declaration -> MultipleErrors
    f (PositionedDeclaration pos _ dec) = addHint (PositionedError pos) (f dec)
    f dec@(ValueDeclaration name _ _ _) = addHint (ErrorInValueDeclaration name) (warningsInDecl moduleNames dec <> checkTypeVarsInDecl dec)
    f (TypeDeclaration name ty) = addHint (ErrorInTypeDeclaration name) (checkTypeVars ty)
    f dec = warningsInDecl moduleNames dec <> checkTypeVarsInDecl dec

    stepD :: S.Set Ident -> Declaration -> MultipleErrors
    stepD _ (TypeClassDeclaration name _ _ decls) = foldMap go decls
      where
      go :: Declaration -> MultipleErrors
      go (PositionedDeclaration _ _ d') = go d'
      go (TypeDeclaration op@(Op _) _)  = errorMessage (ClassOperator name op)
      go _ = mempty
    stepD _ _ = mempty

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

  checkTypeVarsInDecl :: Declaration -> MultipleErrors
  checkTypeVarsInDecl d = let (f, _, _, _, _) = accumTypes checkTypeVars in f d

  checkTypeVars :: Type -> MultipleErrors
  checkTypeVars ty = everythingWithContextOnTypes S.empty mempty mappend step ty <> findUnused ty
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
