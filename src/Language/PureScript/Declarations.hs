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

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment

import qualified Data.Data as D
import Data.Generics (mkQ, everything)

-- |
-- A precedence level for an infix operator
--
type Precedence = Integer

-- |
-- Associativity for infix operators
--
data Associativity = Infixl | Infixr | Infix deriving (D.Data, D.Typeable)

instance Show Associativity where
  show Infixl = "infixl"
  show Infixr = "infixr"
  show Infix  = "infix"

-- |
-- Source position information
--
data SourcePos = SourcePos
  { -- |
    -- Source name
    --
    sourceName :: String
    -- |
    -- Line number
    --
  , sourcePosLine :: Int
    -- |
    -- Column number
    --
  , sourcePosColumn :: Int
  } deriving (D.Data, D.Typeable)

instance Show SourcePos where
  show sp = (sourceName sp) ++ " line " ++ show (sourcePosLine sp) ++ ", column " ++ show (sourcePosColumn sp)

-- |
-- Fixity data for infix operators
--
data Fixity = Fixity Associativity Precedence deriving (Show, D.Data, D.Typeable)

-- |
-- A module declaration, consisting of a module name, a list of declarations, and a list of the
-- declarations that are explicitly exported. If the export list is Nothing, everything is exported.
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
  -- |
  -- A declaration reference with source position information
  --
  | PositionedDeclarationRef SourcePos DeclarationRef
  deriving (Show, D.Data, D.Typeable)

instance Eq DeclarationRef where
  (TypeRef name dctors)  == (TypeRef name' dctors') = name == name' && dctors == dctors'
  (ValueRef name)        == (ValueRef name')        = name == name'
  (TypeClassRef name)    == (TypeClassRef name')    = name == name'
  (TypeInstanceRef name) == (TypeInstanceRef name') = name == name'
  (PositionedDeclarationRef _ r) == r' = r == r'
  r == (PositionedDeclarationRef _ r') = r == r'
  _ == _ = False

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
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration ProperName [String] [(Qualified ProperName, [Type])] [Declaration]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration Ident [(Qualified ProperName, [Type])] (Qualified ProperName) [Type] [Declaration]
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration SourcePos Declaration
  deriving (Show, D.Data, D.Typeable)

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration -> Bool
isValueDecl ValueDeclaration{} = True
isValueDecl (PositionedDeclaration _ d) = isValueDecl d
isValueDecl _ = False

-- |
-- Test if a declaration is a data type or type synonym declaration
--
isDataDecl :: Declaration -> Bool
isDataDecl DataDeclaration{} = True
isDataDecl TypeSynonymDeclaration{} = True
isDataDecl (PositionedDeclaration _ d) = isDataDecl d
isDataDecl _ = False

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration -> Bool
isImportDecl ImportDeclaration{} = True
isImportDecl (PositionedDeclaration _ d) = isImportDecl d
isImportDecl _ = False

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration -> Bool
isExternDataDecl ExternDataDeclaration{} = True
isExternDataDecl (PositionedDeclaration _ d) = isExternDataDecl d
isExternDataDecl _ = False

-- |
-- Test if a declaration is a type class instance foreign import
--
isExternInstanceDecl :: Declaration -> Bool
isExternInstanceDecl ExternInstanceDeclaration{} = True
isExternInstanceDecl (PositionedDeclaration _ d) = isExternInstanceDecl d
isExternInstanceDecl _ = False

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration -> Bool
isFixityDecl FixityDeclaration{} = True
isFixityDecl (PositionedDeclaration _ d) = isFixityDecl d
isFixityDecl _ = False

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration -> Bool
isExternDecl ExternDeclaration{} = True
isExternDecl (PositionedDeclaration _ d) = isExternDecl d
isExternDecl _ = False

-- |
-- Test if a declaration is a type class or instance declaration
--
isTypeClassDeclaration :: Declaration -> Bool
isTypeClassDeclaration TypeClassDeclaration{} = True
isTypeClassDeclaration TypeInstanceDeclaration{} = True
isTypeClassDeclaration (PositionedDeclaration _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard = Value

-- |
-- Data type for values
--
data Value
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Bool
  -- |
  -- A prefix -, will be desugared
  --
  | UnaryMinus Value
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | BinaryNoParens (Qualified Ident) Value Value
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | Parens Value
  -- |
  -- An array literal
  --
  | ArrayLiteral [Value]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(String, Value)]
  -- |
  -- An record property accessor expression
  --
  | Accessor String Value
  -- |
  -- Partial record update
  --
  | ObjectUpdate Value [(String, Value)]
  -- |
  -- Function introduction
  --
  | Abs (Either Ident Binder) Value
  -- |
  -- Function application
  --
  | App Value Value
  -- |
  -- Variable
  --
  | Var (Qualified Ident)
  -- |
  -- Conditional (if-then-else expression)
  --
  | IfThenElse Value Value Value
  -- |
  -- A data constructor
  --
  | Constructor (Qualified ProperName)
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case [Value] [CaseAlternative]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Bool Value Type
  -- |
  -- A let binding
  --
  | Let [Declaration] Value
  -- |
  -- A do-notation block
  --
  | Do [DoNotationElement]
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary Bool (Qualified ProperName, [Type]) (Maybe [TypeClassDictionaryInScope])
  -- |
  -- A value with source position information
  --
  | PositionedValue SourcePos Value deriving (Show, D.Data, D.Typeable)

-- |
-- An alternative in a case statement
--
data CaseAlternative = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder]
    -- |
    -- An optional guard
    --
  , caseAlternativeGuard :: Maybe Guard
    -- |
    -- The result expression
    --
  , caseAlternativeResult :: Value
  } deriving (Show, D.Data, D.Typeable)

-- |
-- Find the original dictionary which a type class dictionary in scope refers to
--
canonicalizeDictionary :: TypeClassDictionaryInScope -> Qualified Ident
canonicalizeDictionary (TypeClassDictionaryInScope { tcdType = TCDRegular, tcdName = nm }) = nm
canonicalizeDictionary (TypeClassDictionaryInScope { tcdType = TCDAlias nm }) = nm

-- |
-- A statement in a do-notation block
--
data DoNotationElement
  -- |
  -- A monadic value without a binder
  --
  = DoNotationValue Value
  -- |
  -- A monadic value with a binder
  --
  | DoNotationBind Binder Value
  -- |
  -- A let statement, i.e. a pure value with a binder
  --
  | DoNotationLet [Declaration]
  -- |
  -- A do notation element with source position information
  --
  | PositionedDoNotationElement SourcePos DoNotationElement deriving (Show, D.Data, D.Typeable)

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a boolean literal
  --
  | BooleanBinder Bool
  -- |
  -- A binder which matches a string literal
  --
  | StringBinder String
  -- |
  -- A binder which matches a numeric literal
  --
  | NumberBinder (Either Integer Double)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified ProperName) [Binder]
  -- |
  -- A binder which matches a record and binds its properties
  --
  | ObjectBinder [(String, Binder)]
  -- |
  -- A binder which matches an array and binds its elements
  --
  | ArrayBinder [Binder]
  -- |
  -- A binder which matches an array and binds its head and tail
  --
  | ConsBinder Binder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourcePos Binder deriving (Show, D.Data, D.Typeable)

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: (D.Data d) => d -> [Ident]
binderNames = everything (++) (mkQ [] go)
  where
  go (VarBinder ident) = [ident]
  go (NamedBinder ident _) = [ident]
  go _ = []
