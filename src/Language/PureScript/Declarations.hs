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
-- | Data types for modules and declarations
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

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr deriving (Show, D.Data, D.Typeable)

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence deriving (Show, D.Data, D.Typeable)

-- |
-- A module declaration, consisting of a module name and a list of declarations
--
data Module = Module ModuleName [Declaration] deriving (Show, D.Data, D.Typeable)

-- |
-- The type of a foreign import
--
data ForeignImportType
  -- |
  -- A regular foreign import
  --
  = ForeignImport
  -- |
  -- A foreign import which contains inline Javascript as a string literal
  --
  | InlineJavascript
  -- |
  -- A type class dictionary import, generated during desugaring of type class declarations
  --
  | TypeClassDictionaryImport
  -- |
  -- A type class dictionary member accessor import, generated during desugaring of type class declarations
  --
  | TypeClassAccessorImport deriving (Show, Eq, D.Data, D.Typeable)

-- |
-- An item in a list of explicit imports
--
data ImportType
  -- |
  -- A type constructor import
  --
  = TypeImport ProperName
  -- |
  -- A declaration import
  --
  | NameImport Ident
  deriving (Show, D.Data, D.Typeable)

-- |
-- The data type of declarations
--
data Declaration
  -- |
  -- A data type declaration (name, arguments, data constructors)
  --
  = DataDeclaration ProperName [String] [(ProperName, [Type])]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration [Declaration]
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration ProperName [String] Type
  -- |
  -- A type declaration for a value (name, ty)
  --
  | TypeDeclaration Ident Type
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
  | ValueDeclaration Ident [Binder] (Maybe Guard) Value
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration [(Ident, Value)]
  -- |
  -- A foreign import declaration (type, name, optional inline Javascript, type)
  --
  | ExternDeclaration ForeignImportType Ident (Maybe JS) Type
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration ProperName Kind
  -- |
  -- A fixity declaration (fixity data, operator name)
  --
  | FixityDeclaration Fixity String
  -- |
  -- A module import (module name, optional set of identifiers to import)
  --
  | ImportDeclaration ModuleName (Maybe [ImportType])
  -- |
  -- A type class declaration (name, argument, member declarations)
  --
  | TypeClassDeclaration ProperName [String] [Declaration]
  -- |
  -- A type instance declaration (dependencies, class name, instance type, member declarations)
  --
  | TypeInstanceDeclaration [(Qualified ProperName, [Type])] (Qualified ProperName) [Type] [Declaration]
  deriving (Show, D.Data, D.Typeable)

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration -> Bool
isValueDecl ValueDeclaration{} = True
isValueDecl _ = False

-- |
-- Test if a declaration is a data type or type synonym declaration
--
isDataDecl :: Declaration -> Bool
isDataDecl DataDeclaration{} = True
isDataDecl TypeSynonymDeclaration{} = True
isDataDecl _ = False

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration -> Bool
isImportDecl ImportDeclaration{} = True
isImportDecl _ = False

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration -> Bool
isExternDataDecl ExternDataDeclaration{} = True
isExternDataDecl _ = False

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration -> Bool
isFixityDecl FixityDeclaration{} = True
isFixityDecl _ = False

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration -> Bool
isExternDecl ExternDeclaration{} = True
isExternDecl _ = False

-- |
-- Test if a declaration is a type class or instance declaration
--
isTypeClassDeclaration :: Declaration -> Bool
isTypeClassDeclaration TypeClassDeclaration{} = True
isTypeClassDeclaration TypeInstanceDeclaration{} = True
isTypeClassDeclaration _ = False

-- |
-- Extracts names from a list of explicit imports.
--
nameImports :: [ImportType] -> [Ident]
nameImports ((NameImport ident):xs) = ident : nameImports xs
nameImports (_ : xs) = nameImports xs
nameImports _ = []

-- |
-- Extracts types from a list of explicit imports.
--
typeImports :: [ImportType] -> [ProperName]
typeImports (TypeImport ident : xs) = ident : typeImports xs
typeImports (_ : xs) = typeImports xs
typeImports _ = []

-- |
-- Extracts classes from a list of explicit imports.
-- note: class imports are indistinguishable from types at the moment
--
classImports :: [ImportType] -> [ProperName]
classImports = typeImports
