-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CaseDeclarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level binders with
-- case expressions.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Sugar.CaseDeclarations (
    desugarCases,
    desugarCasesModule
) where

import Data.List (nub, groupBy)

import Control.Applicative
import Control.Monad ((<=<), forM, join, unless, replicateM)
import Control.Monad.Except (throwError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Supply.Class

import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Traversals
import Language.PureScript.TypeChecker.Monad (guardWith)

-- Data.Either.isLeft (base 4.7)
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- |
-- Replace all top-level binders in a module with case expressions.
--
desugarCasesModule :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [Module] -> m [Module]
desugarCasesModule ms = forM ms $ \(Module coms name ds exps) ->
  rethrow (onErrorMessages (ErrorInModule name)) $
    Module coms name <$> (desugarCases <=< desugarAbs $ ds) <*> pure exps

desugarAbs :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarAbs = flip parU f
  where
  (f, _, _) = everywhereOnValuesM return replace return

  replace :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => Expr -> m Expr
  replace (Abs (Right binder) val) = do
    ident <- Ident <$> freshName
    return $ Abs (Left ident) $ Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] (Right val)]
  replace other = return other

-- |
-- Replace all top-level binders with case expressions.
--
desugarCases :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
desugarCases = desugarRest <=< fmap join . flip parU toDecls . groupBy inSameGroup
  where
    desugarRest :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
    desugarRest (TypeInstanceDeclaration name constraints className tys ds : rest) =
      (:) <$> (TypeInstanceDeclaration name constraints className tys <$> desugarCases ds) <*> desugarRest rest
    desugarRest (ValueDeclaration name nameKind bs result : rest) =
      let (_, f, _) = everywhereOnValuesTopDownM return go return
          f' (Left gs) = Left <$> mapM (pairM return f) gs
          f' (Right v) = Right <$> f v
      in (:) <$> (ValueDeclaration name nameKind bs <$> f' result) <*> desugarRest rest
      where
      go (Let ds val') = Let <$> desugarCases ds <*> pure val'
      go other = return other
    desugarRest (PositionedDeclaration pos com d : ds) = do
      (d' : ds') <- desugarRest (d : ds)
      return (PositionedDeclaration pos com d' : ds')
    desugarRest (d : ds) = (:) d <$> desugarRest ds
    desugarRest [] = pure []

inSameGroup :: Declaration -> Declaration -> Bool
inSameGroup (ValueDeclaration ident1 _ _ _) (ValueDeclaration ident2 _ _ _) = ident1 == ident2
inSameGroup (PositionedDeclaration _ _ d1) d2 = inSameGroup d1 d2
inSameGroup d1 (PositionedDeclaration _ _ d2) = inSameGroup d1 d2
inSameGroup _ _ = False

toDecls :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => [Declaration] -> m [Declaration]
toDecls [ValueDeclaration ident nameKind bs (Right val)] | all isVarBinder bs = do
  let args = map (\(VarBinder arg) -> arg) bs
      body = foldr (Abs . Left) val args
  guardWith (errorMessage (OverlappingArgNames (Just ident))) $ length (nub args) == length args
  return [ValueDeclaration ident nameKind [] (Right body)]
toDecls ds@(ValueDeclaration ident _ bs result : _) = do
  let tuples = map toTuple ds
  unless (all ((== length bs) . length . fst) tuples) $
      throwError . errorMessage $ ArgListLengthsDiffer ident
  unless (not (null bs) || isLeft result) $
      throwError . errorMessage $ DuplicateValueDeclaration ident
  caseDecl <- makeCaseDeclaration ident tuples
  return [caseDecl]
toDecls (PositionedDeclaration pos com d : ds) = do
  (d' : ds') <- rethrowWithPosition pos $ toDecls (d : ds)
  return (PositionedDeclaration pos com d' : ds')
toDecls ds = return ds

isVarBinder :: Binder -> Bool
isVarBinder (VarBinder _) = True
isVarBinder _ = False

toTuple :: Declaration -> ([Binder], Either [(Guard, Expr)] Expr)
toTuple (ValueDeclaration _ _ bs result) = (bs, result)
toTuple (PositionedDeclaration _ _ d) = toTuple d
toTuple _ = error "Not a value declaration"

makeCaseDeclaration :: (Functor m, Applicative m, MonadSupply m, MonadError MultipleErrors m) => Ident -> [([Binder], Either [(Guard, Expr)] Expr)] -> m Declaration
makeCaseDeclaration ident alternatives = do
  let argPattern = length . fst . head $ alternatives
  args <- map Ident <$> replicateM argPattern freshName
  let
    vars = map (Var . Qualified Nothing) args
    binders = [ CaseAlternative bs result | (bs, result) <- alternatives ]
    value = foldr (Abs . Left) (Case vars binders) args
  return $ ValueDeclaration ident Value [] (Right value)

