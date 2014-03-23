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

-- |
-- Replace all top level type declarations in a module with type annotations
--
desugarTypeDeclarationsModule :: [Module] -> Either String [Module]
desugarTypeDeclarationsModule ms = forM ms $ \(Module name ds exps) -> Module name <$> desugarTypeDeclarations ds <*> pure exps

-- |
-- Replace all top level type declarations with type annotations
--
desugarTypeDeclarations :: [Declaration] -> Either String [Declaration]
desugarTypeDeclarations (TypeDeclaration name ty : ValueDeclaration name' nameKind [] Nothing val : rest) | name == name' =
  desugarTypeDeclarations (ValueDeclaration name nameKind [] Nothing (TypedValue True val ty) : rest)
desugarTypeDeclarations (TypeDeclaration name _ : _) = throwError $ "Orphan type declaration for " ++ show name
desugarTypeDeclarations (ValueDeclaration name nameKind bs g val : rest) = do
  (:) <$> (ValueDeclaration name nameKind bs g <$> everywhereM' (mkM go) val) <*> desugarTypeDeclarations rest
  where
  go (Let ds val') = Let <$> desugarTypeDeclarations ds <*> pure val'
  go other = return other
desugarTypeDeclarations (d:ds) = (:) d <$> desugarTypeDeclarations ds
desugarTypeDeclarations [] = return []
