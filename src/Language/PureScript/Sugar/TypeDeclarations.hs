-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeDeclarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces top-level type declarations with
-- type annotations on the corresponding expression.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.TypeDeclarations (
    desugarTypeDeclarations,
    desugarTypeDeclarationsModule
) where

import Data.Generics (mkM)
import Data.Generics.Extras

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad (forM)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all top level type declarations in a module with type annotations
--
desugarTypeDeclarationsModule :: [Module] -> Either ErrorStack [Module]
desugarTypeDeclarationsModule ms = forM ms $ \(Module name ds exps) -> Module name <$> desugarTypeDeclarations ds <*> pure exps

-- |
-- Replace all top level type declarations with type annotations
--
desugarTypeDeclarations :: [Declaration] -> Either ErrorStack [Declaration]
desugarTypeDeclarations (PositionedDeclaration pos d : ds) = do
  (d' : ds') <- desugarTypeDeclarations (d : ds)
  return (PositionedDeclaration pos d' : ds')
desugarTypeDeclarations (TypeDeclaration name ty : d : rest) = do
  (_, nameKind, val) <- fromValueDeclaration d
  desugarTypeDeclarations (ValueDeclaration name nameKind [] Nothing (TypedValue True val ty) : rest)
  where
  fromValueDeclaration :: Declaration -> Either ErrorStack (Ident, NameKind, Value)
  fromValueDeclaration (ValueDeclaration name' nameKind [] Nothing val) | name == name' = return (name', nameKind, val)
  fromValueDeclaration (PositionedDeclaration pos d') = do
    (ident, nameKind, val) <- fromValueDeclaration d'
    return (ident, nameKind, PositionedValue pos val)
  fromValueDeclaration _ = throwError $ mkErrorStack ("Orphan type declaration for " ++ show name) Nothing
desugarTypeDeclarations (ValueDeclaration name nameKind bs g val : rest) = do
  (:) <$> (ValueDeclaration name nameKind bs g <$> everywhereM' (mkM go) val) <*> desugarTypeDeclarations rest
  where
  go (Let ds val') = Let <$> desugarTypeDeclarations ds <*> pure val'
  go other = return other
desugarTypeDeclarations (d:ds) = (:) d <$> desugarTypeDeclarations ds
desugarTypeDeclarations [] = return []
