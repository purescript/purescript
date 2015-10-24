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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Linter (lint, module L) where

import Data.List (mapAccumL, nub, (\\))
import Data.Maybe (mapMaybe)
import Data.Monoid

import qualified Data.Set as S

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Writer.Class

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Types
import Language.PureScript.Linter.Exhaustive as L

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
  lintDeclaration d =
    let (f, _, _, _, _) = everythingWithContextOnValues moduleNames mempty mappend stepD stepE stepB def def

        f' :: Declaration -> MultipleErrors
        f' (PositionedDeclaration pos _ dec) = addHint (PositionedError pos) (f' dec)
        f' dec@(ValueDeclaration name _ _ _) = addHint (ErrorInValueDeclaration name) (f dec <> checkTypeVarsInDecl dec)
        f' (TypeDeclaration name ty) = addHint (ErrorInTypeDeclaration name) (checkTypeVars ty)
        f' dec = f dec <> checkTypeVarsInDecl dec

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
    stepE s (Abs (Left name) _) = bindName s name
    stepE s (Let ds' _) =
      case mapAccumL bindName s (nub (mapMaybe getDeclIdent ds')) of
        (s', es) -> (s', mconcat es)
    stepE s _ = (s, mempty)

    stepB :: S.Set Ident -> Binder -> (S.Set Ident, MultipleErrors)
    stepB s (VarBinder name) = bindName s name
    stepB s (NamedBinder name _) = bindName s name
    stepB s (TypedBinder _ b) = stepB s b
    stepB s _ = (s, mempty)

    bindName :: S.Set Ident -> Ident -> (S.Set Ident, MultipleErrors)
    bindName = bind ShadowedName

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
