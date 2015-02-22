-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Subsumption
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Subsumption checking
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeChecker.Subsumption (
    subsumes
) where

import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Monad.Except
import Control.Monad.Unify

import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.Types

-- |
-- Check whether one type subsumes another, rethrowing errors to provide a better error message
--
subsumes :: Maybe Expr -> Type -> Type -> UnifyT Type Check (Maybe Expr)
subsumes val ty1 ty2 = rethrow (onErrorMessages (ErrorInSubsumption ty1 ty2)) $ subsumes' val ty1 ty2

-- |
-- Check whether one type subsumes another
--
subsumes' :: Maybe Expr -> Type -> Type -> UnifyT Type Check (Maybe Expr)
subsumes' val (ForAll ident ty1 _) ty2 = do
  replaced <- replaceVarWithUnknown ident ty1
  subsumes val replaced ty2
subsumes' val ty1 (ForAll ident ty2 sco) =
  case sco of
    Just sco' -> do
      sko <- newSkolemConstant
      let sk = skolemize ident sko sco' ty2
      subsumes val ty1 sk
    Nothing -> throwError . errorMessage $ UnspecifiedSkolemScope
subsumes' val (TypeApp (TypeApp f1 arg1) ret1) (TypeApp (TypeApp f2 arg2) ret2) | f1 == tyFunction && f2 == tyFunction = do
  _ <- subsumes Nothing arg2 arg1
  _ <- subsumes Nothing ret1 ret2
  return val
subsumes' val (SaturatedTypeSynonym name tyArgs) ty2 = do
  ty1 <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  subsumes val ty1 ty2
subsumes' val ty1 (SaturatedTypeSynonym name tyArgs) = do
  ty2 <- introduceSkolemScope <=< expandTypeSynonym name $ tyArgs
  subsumes val ty1 ty2
subsumes' val (KindedType ty1 _) ty2 =
  subsumes val ty1 ty2
subsumes' val ty1 (KindedType ty2 _) =
  subsumes val ty1 ty2
subsumes' (Just val) (ConstrainedType constraints ty1) ty2 = do
  dicts <- getTypeClassDictionaries
  subsumes' (Just $ foldl App val (map (flip (TypeClassDictionary True) dicts) constraints)) ty1 ty2
subsumes' val (TypeApp f1 r1) (TypeApp f2 r2) | f1 == tyObject && f2 == tyObject = do
  let
    (ts1, r1') = rowToList r1
    (ts2, r2') = rowToList r2
    ts1' = sortBy (comparing fst) ts1
    ts2' = sortBy (comparing fst) ts2
  go ts1' ts2' r1' r2'
  return val
  where
  go [] ts2 r1' r2' = r1' =?= rowFromList (ts2, r2')
  go ts1 [] r1' r2' = r2' =?= rowFromList (ts1, r1')
  go ((p1, ty1) : ts1) ((p2, ty2) : ts2) r1' r2'
    | p1 == p2 = do _ <- subsumes Nothing ty1 ty2
                    go ts1 ts2 r1' r2'
    | p1 < p2 = do rest <- fresh
                   r2' =?= RCons p1 ty1 rest
                   go ts1 ((p2, ty2) : ts2) r1' rest
    | otherwise = do rest <- fresh
                     r1' =?= RCons p2 ty2 rest
                     go ((p1, ty1) : ts1) ts2 rest r2'
subsumes' val ty1 ty2@(TypeApp obj _) | obj == tyObject = subsumes val ty2 ty1
subsumes' val ty1 ty2 = do
  ty1 =?= ty2
  return val
