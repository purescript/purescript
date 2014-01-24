-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Declarations
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

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Declarations where

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.CodeGen.JS.AST

import qualified Data.Data as D

type Precedence = Integer

data Associativity = Infixl | Infixr deriving (Show, D.Data, D.Typeable)

data Fixity = Fixity Associativity Precedence deriving (Show, D.Data, D.Typeable)

data Module = Module ProperName [Declaration] deriving (Show, D.Data, D.Typeable)

data ForeignImportType
  = ForeignImport
  | TypeClassDictionaryImport
  | TypeClassAccessorImport deriving (Show, Eq, D.Data, D.Typeable)

data Declaration
  = DataDeclaration ProperName [String] [(ProperName, Maybe Type)]
  | DataBindingGroupDeclaration [Declaration]
  | TypeSynonymDeclaration ProperName [String] Type
  | TypeDeclaration Ident Type
  | ValueDeclaration Ident [[Binder]] (Maybe Guard) Value
  | BindingGroupDeclaration [(Ident, Value)]
  | ExternDeclaration ForeignImportType Ident (Maybe JS) Type
  | ExternDataDeclaration ProperName Kind
  | FixityDeclaration Fixity String
  | ImportDeclaration ModuleName (Maybe [Either Ident ProperName])
  | TypeClassDeclaration ProperName String [Declaration]
  | TypeInstanceDeclaration [(Qualified ProperName, Type)] (Qualified ProperName) Type [Declaration]
  deriving (Show, D.Data, D.Typeable)

isValueDecl :: Declaration -> Bool
isValueDecl (ValueDeclaration _ _ _ _) = True
isValueDecl _ = False

isDataDecl :: Declaration -> Bool
isDataDecl (DataDeclaration _ _ _) = True
isDataDecl (TypeSynonymDeclaration _ _ _) = True
isDataDecl _ = False

isImportDecl :: Declaration -> Bool
isImportDecl (ImportDeclaration _ _) = True
isImportDecl _ = False

isExternDataDecl :: Declaration -> Bool
isExternDataDecl (ExternDataDeclaration _ _) = True
isExternDataDecl _ = False

isFixityDecl :: Declaration -> Bool
isFixityDecl (FixityDeclaration _ _) = True
isFixityDecl _ = False

isExternDecl :: Declaration -> Bool
isExternDecl (ExternDeclaration _ _ _ _) = True
isExternDecl _ = False

isTypeClassDeclaration :: Declaration -> Bool
isTypeClassDeclaration (TypeClassDeclaration _ _ _) = True
isTypeClassDeclaration (TypeInstanceDeclaration _ _ _ _) = True
isTypeClassDeclaration _ = False
