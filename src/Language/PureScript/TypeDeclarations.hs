-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeDeclarations
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeDeclarations (
    desugarTypeDeclarations
) where

import Control.Applicative
import Control.Monad.Error.Class

import Language.PureScript.Declarations
import Language.PureScript.Values

desugarTypeDeclarations :: [Declaration] -> Either String [Declaration]
desugarTypeDeclarations (TypeDeclaration name ty : ValueDeclaration name' [] Nothing val : rest) | name == name' =
  desugarTypeDeclarations (ValueDeclaration name [] Nothing (TypedValue val ty) : rest)
desugarTypeDeclarations (TypeDeclaration name _ : _) = throwError $ "Orphan type declaration for " ++ show name
desugarTypeDeclarations (ModuleDeclaration name ds:ds') = (:) <$> (ModuleDeclaration name <$> desugarTypeDeclarations ds)
                                                              <*> desugarTypeDeclarations ds'
desugarTypeDeclarations (d:ds) = (:) d <$> desugarTypeDeclarations ds
desugarTypeDeclarations [] = return []
