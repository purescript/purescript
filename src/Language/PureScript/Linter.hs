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

module Language.PureScript.Linter (lint, module L) where

import Data.List (mapAccumL, nub)
import Data.Monoid

import qualified Data.Set as S

import Control.Applicative
import Control.Monad.Writer.Class

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Linter.Exhaustive as L

-- | Lint the PureScript AST.
-- |
-- | Right now, this pass only performs a shadowing check.
lint :: forall m. (Applicative m, MonadWriter MultipleErrors m) => Module -> m ()
lint (Module _ mn ds _) = censor (onErrorMessages (ErrorInModule mn)) $ mapM_ lintDeclaration ds
  where
  moduleNames :: S.Set Ident
  moduleNames = S.fromList (nub (concatMap getDeclIdent ds))

  getDeclIdent :: Declaration -> [Ident]
  getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _) = [ident]
  getDeclIdent (ExternDeclaration ident _) = [ident]
  getDeclIdent (ExternInstanceDeclaration ident _ _ _) = [ident]
  getDeclIdent (TypeInstanceDeclaration ident _ _ _ _) = [ident]
  getDeclIdent (BindingGroupDeclaration bs) = map (\(ident, _, _) -> ident) bs
  getDeclIdent _ = []

  lintDeclaration :: Declaration -> m ()
  lintDeclaration d =
    let (f, _, _, _, _) = everythingWithContextOnValues moduleNames mempty mappend stepD stepE stepB def def

        f' :: Declaration -> MultipleErrors
        f' (PositionedDeclaration pos _ dec) = onErrorMessages (PositionedError pos) (f' dec)
        f' dec = f dec

    in tell (f' d)
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
      case mapAccumL bind s (nub (concatMap getDeclIdent ds')) of
        (s', es) -> (s', mconcat es)
    stepE s _ = (s, mempty)

    stepB :: S.Set Ident -> Binder -> (S.Set Ident, MultipleErrors)
    stepB s (VarBinder name) = bind s name
    stepB s (NamedBinder name _) = bind s name
    stepB s _ = (s, mempty)

    bind :: S.Set Ident -> Ident -> (S.Set Ident, MultipleErrors)
    bind s name | name `S.member` s = (s, errorMessage (ShadowedName name))
                | otherwise = (S.insert name s, mempty)
