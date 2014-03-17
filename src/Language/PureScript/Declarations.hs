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
import Language.PureScript.Environment

import qualified Data.Data as D

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr deriving (D.Data, D.Typeable)

instance Show Associativity where
  show Infixl = "infixl"
  show Infixr = "infixr"

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence deriving (Show, D.Data, D.Typeable)

-- |
-- A module declaration, consisting of a module name, a list of declarations, and a list of the
-- declarations that are explicitly imported. If the export list is Nothing, everything is exported.
--
data Module = Module ModuleName [Declaration] (Maybe [DeclarationRef]) deriving (Show, D.Data, D.Typeable)

-- |
-- An item in a list of explicit imports or exports
--
data DeclarationRef
  -- |
  -- A type constructor with data constructors
  --
  = TypeRef ProperName (Maybe [ProperName])
  -- |
  -- A value
  --
  | ValueRef Ident
  -- |
  -- A type class
  --
  | TypeClassRef ProperName
    -- |
  -- A type class instance, created during typeclass desugaring (name, class name, instance types)
  --
  | TypeInstanceRef Ident
  deriving (Show, Eq, D.Data, D.Typeable)

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
  | ValueDeclaration Ident NameKind [Binder] (Maybe Guard) Value
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration [(Ident, NameKind, Value)]
  -- |
  -- A foreign import declaration (type, name, optional inline Javascript, type)
  --
  | ExternDeclaration ForeignImportType Ident (Maybe JS) Type
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration ProperName Kind
  -- |
  -- A type class instance foreign import
  --
  | ExternInstanceDeclaration Ident [(Qualified ProperName, [Type])] (Qualified ProperName) [Type]
  -- |
  -- A fixity declaration (fixity data, operator name)
  --
  | FixityDeclaration Fixity String
  -- |
  -- A module import (module name, optional set of identifiers to import, optional "qualified as"
  -- name)
  --
  | ImportDeclaration ModuleName (Maybe [DeclarationRef]) (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, member declarations)
  --
  | TypeClassDeclaration ProperName [String] [Declaration]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration Ident [(Qualified ProperName, [Type])] (Qualified ProperName) [Type] [Declaration]
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
-- Test if a declaration is a type class instance foreign import
--
isExternInstanceDecl :: Declaration -> Bool
isExternInstanceDecl ExternInstanceDeclaration{} = True
isExternInstanceDecl _ = False

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
