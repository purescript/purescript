-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Linter
-- Copyright   :  (c) Copyright 2015 PureScript
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | This module implements a simple linting pass on the PureScript AST.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Linter (lint) where

import Data.List (mapAccumL, nub)
import Data.Maybe (mapMaybe)
import Data.Monoid

import qualified Data.Set as S

import Control.Applicative
import Control.Monad.Writer.Class

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors

-- | Lint the PureScript AST.
-- |
-- | Right now, this pass only performs a shadowing check.
lint :: forall m. (Applicative m, MonadWriter MultipleErrors m) => Module -> m ()
lint (Module _ mn ds _) = censor (onErrorMessages (ErrorInModule mn)) $ mapM_ lintDeclaration ds
  where
  moduleNames :: S.Set Ident
  moduleNames = S.fromList (nub (mapMaybe getDeclIdent ds))

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _) = Just ident
  getDeclIdent (ExternDeclaration ident _) = Just ident
  getDeclIdent (ExternInstanceDeclaration ident _ _ _) = Just ident
  getDeclIdent (TypeInstanceDeclaration ident _ _ _ _) = Just ident
  getDeclIdent (BindingGroupDeclaration _) = error "lint: binding groups should not be desugared yet."
  getDeclIdent _ = Nothing

  lintDeclaration :: Declaration -> m ()
  lintDeclaration d =
    let (f, _, _, _, _) = everythingWithContextOnValues moduleNames mempty mappend stepD stepE stepB def def
    in tell (f d)
    where
    def s _ = (s, mempty)

    stepD :: S.Set Ident -> Declaration -> (S.Set Ident, MultipleErrors)
    stepD s (TypeClassDeclaration name _ _ decls) = (s, foldr go mempty decls)
      where
      go :: Declaration -> MultipleErrors -> MultipleErrors
      go (PositionedDeclaration _ _ d') errs = go d' errs
      go (TypeDeclaration op@(Op _) _) errs = errorMessage (ClassOperator name op) <> errs
      go _ errs = errs
    stepD s _ = (s, mempty)

    stepE :: S.Set Ident -> Expr -> (S.Set Ident, MultipleErrors)
    stepE s (Abs (Left name) _) = bind s name
    stepE s (Let ds' _) =
      case mapAccumL bind s (nub (mapMaybe getDeclIdent ds')) of
        (s', es) -> (s', mconcat es)
    stepE s _ = (s, mempty)

    stepB :: S.Set Ident -> Binder -> (S.Set Ident, MultipleErrors)
    stepB s (VarBinder name) = bind s name
    stepB s (NamedBinder name _) = bind s name
    stepB s _ = (s, mempty)

    bind :: S.Set Ident -> Ident -> (S.Set Ident, MultipleErrors)
    bind s name | name `S.member` s = (s, errorMessage (ShadowedName name))
                | otherwise = (S.insert name s, mempty)
