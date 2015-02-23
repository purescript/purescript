-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Declarations
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Data types for modules and declarations
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Language.PureScript.AST.Declarations where

import qualified Data.Data as D

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Operators
import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Comments
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment

-- |
-- A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
--
data Module = Module [Comment] ModuleName [Declaration] (Maybe [DeclarationRef]) deriving (Show, D.Data, D.Typeable)

-- | Return a module's name.
getModuleName :: Module -> ModuleName
getModuleName (Module _ name _ _) = name

-- |
-- Test if a declaration is exported, given a module's export list.
--
isExported :: Maybe [DeclarationRef] -> Declaration -> Bool
isExported Nothing _ = True
isExported _ TypeInstanceDeclaration{} = True
isExported exps (PositionedDeclaration _ _ d) = isExported exps d
isExported (Just exps) decl = any (matches decl) exps
  where
  matches (TypeDeclaration ident _) (ValueRef ident') = ident == ident'
  matches (ValueDeclaration ident _ _ _) (ValueRef ident') = ident == ident'
  matches (ExternDeclaration _ ident _ _) (ValueRef ident') = ident == ident'
  matches (DataDeclaration _ ident _ _) (TypeRef ident' _) = ident == ident'
  matches (ExternDataDeclaration ident _) (TypeRef ident' _) = ident == ident'
  matches (TypeSynonymDeclaration ident _ _) (TypeRef ident' _) = ident == ident'
  matches (TypeClassDeclaration ident _ _ _) (TypeClassRef ident') = ident == ident'
  matches (PositionedDeclaration _ _ d) r = d `matches` r
  matches d (PositionedDeclarationRef _ _ r) = d `matches` r
  matches _ _ = False

exportedDeclarations :: Module -> [Declaration]
exportedDeclarations (Module _ _ decls exps) = filter (isExported exps) (flattenDecls decls)

-- |
-- Test if a data constructor for a given type is exported, given a module's export list.
--
isDctorExported :: ProperName -> Maybe [DeclarationRef] -> ProperName -> Bool
isDctorExported _ Nothing _ = True
isDctorExported ident (Just exps) ctor = test `any` exps
  where
  test (PositionedDeclarationRef _ _ d) = test d
  test (TypeRef ident' Nothing) = ident == ident'
  test (TypeRef ident' (Just ctors)) = ident == ident' && ctor `elem` ctors
  test _ = False

-- |
-- Return the exported data constructors for a given type.
--
exportedDctors :: Module -> ProperName -> [ProperName]
exportedDctors (Module _ _ decls exps) ident =
  filter (isDctorExported ident exps) dctors
  where
  dctors = concatMap getDctors (flattenDecls decls)
  getDctors (DataDeclaration _ _ _ ctors) = map fst ctors
  getDctors (PositionedDeclaration _ _ d) = getDctors d
  getDctors _ = []

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
  | PositionedDeclarationRef SourceSpan [Comment] DeclarationRef
  deriving (Show, D.Data, D.Typeable)

instance Eq DeclarationRef where
  (TypeRef name dctors)  == (TypeRef name' dctors') = name == name' && dctors == dctors'
  (ValueRef name)        == (ValueRef name')        = name == name'
  (TypeClassRef name)    == (TypeClassRef name')    = name == name'
  (TypeInstanceRef name) == (TypeInstanceRef name') = name == name'
  (PositionedDeclarationRef _ _ r) == r' = r == r'
  r == (PositionedDeclarationRef _ _ r') = r == r'
  _ == _ = False

-- |
-- The data type which specifies type of import declaration
--
data ImportDeclarationType
  -- |
  -- Unqualified import
  --
  = Unqualified
  -- |
  -- Qualified import with a list of references to import
  --
  | Qualifying [DeclarationRef]
  -- |
  -- Import with hiding clause with a list of references to hide
  --
  | Hiding [DeclarationRef]
  deriving (Show, D.Data, D.Typeable)

-- |
-- The data type of declarations
--
data Declaration
  -- |
  -- A data type declaration (data or newtype, name, arguments, data constructors)
  --
  = DataDeclaration DataDeclType ProperName [(String, Maybe Kind)] [(ProperName, [Type])]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration [Declaration]
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration ProperName [(String, Maybe Kind)] Type
  -- |
  -- A type declaration for a value (name, ty)
  --
  | TypeDeclaration Ident Type
  -- |
  -- A value declaration (name, top-level binders, optional guard, value)
  --
  | ValueDeclaration Ident NameKind [Binder] (Either [(Guard, Expr)] Expr)
  -- |
  -- A minimal mutually recursive set of value declarations
  --
  | BindingGroupDeclaration [(Ident, NameKind, Expr)]
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
  | ExternInstanceDeclaration Ident [Constraint] (Qualified ProperName) [Type]
  -- |
  -- A fixity declaration (fixity data, operator name)
  --
  | FixityDeclaration Fixity String
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
  | ImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration ProperName [(String, Maybe Kind)] [Constraint] [Declaration]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration Ident [Constraint] (Qualified ProperName) [Type] [Declaration]
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration SourceSpan [Comment] Declaration
  deriving (Show, D.Data, D.Typeable)

-- |
-- Test if a declaration is a value declaration
--
isValueDecl :: Declaration -> Bool
isValueDecl ValueDeclaration{} = True
isValueDecl (PositionedDeclaration _ _ d) = isValueDecl d
isValueDecl _ = False

-- |
-- Test if a declaration is a data type or type synonym declaration
--
isDataDecl :: Declaration -> Bool
isDataDecl DataDeclaration{} = True
isDataDecl TypeSynonymDeclaration{} = True
isDataDecl (PositionedDeclaration _ _ d) = isDataDecl d
isDataDecl _ = False

-- |
-- Test if a declaration is a module import
--
isImportDecl :: Declaration -> Bool
isImportDecl ImportDeclaration{} = True
isImportDecl (PositionedDeclaration _ _ d) = isImportDecl d
isImportDecl _ = False

-- |
-- Test if a declaration is a data type foreign import
--
isExternDataDecl :: Declaration -> Bool
isExternDataDecl ExternDataDeclaration{} = True
isExternDataDecl (PositionedDeclaration _ _ d) = isExternDataDecl d
isExternDataDecl _ = False

-- |
-- Test if a declaration is a type class instance foreign import
--
isExternInstanceDecl :: Declaration -> Bool
isExternInstanceDecl ExternInstanceDeclaration{} = True
isExternInstanceDecl (PositionedDeclaration _ _ d) = isExternInstanceDecl d
isExternInstanceDecl _ = False

-- |
-- Test if a declaration is a fixity declaration
--
isFixityDecl :: Declaration -> Bool
isFixityDecl FixityDeclaration{} = True
isFixityDecl (PositionedDeclaration _ _ d) = isFixityDecl d
isFixityDecl _ = False

-- |
-- Test if a declaration is a foreign import
--
isExternDecl :: Declaration -> Bool
isExternDecl ExternDeclaration{} = True
isExternDecl (PositionedDeclaration _ _ d) = isExternDecl d
isExternDecl _ = False

-- |
-- Test if a declaration is a type class instance declaration
--
isTypeClassInstanceDeclaration :: Declaration -> Bool
isTypeClassInstanceDeclaration TypeInstanceDeclaration{} = True
isTypeClassInstanceDeclaration (PositionedDeclaration _ _ d) = isTypeClassInstanceDeclaration d
isTypeClassInstanceDeclaration _ = False

-- |
-- Test if a declaration is a type class declaration
--
isTypeClassDeclaration :: Declaration -> Bool
isTypeClassDeclaration TypeClassDeclaration{} = True
isTypeClassDeclaration (PositionedDeclaration _ _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

-- |
-- Recursively flatten data binding groups in the list of declarations
flattenDecls :: [Declaration] -> [Declaration]
flattenDecls = concatMap flattenOne
    where flattenOne :: Declaration -> [Declaration]
          flattenOne (DataBindingGroupDeclaration decls) = concatMap flattenOne decls
          flattenOne d = [d]

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard = Expr

-- |
-- Data type for expressions and terms
--
data Expr
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
  | UnaryMinus Expr
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | BinaryNoParens Expr Expr Expr
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  --
  | Parens Expr
  -- |
  -- Operator section. This will be removed during desugaring and replaced with a partially applied
  -- operator or lambda to flip the arguments.
  --
  | OperatorSection Expr (Either Expr Expr)
  -- |
  -- An array literal
  --
  | ArrayLiteral [Expr]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(String, Expr)]
  -- |
  -- An object constructor (object literal with underscores). This will be removed during
  -- desugaring and expanded into a lambda that returns an object literal.
  --
  | ObjectConstructor [(String, Maybe Expr)]
  -- |
  -- An object property getter (e.g. `_.x`). This will be removed during
  -- desugaring and expanded into a lambda that reads a property from an object.
  --
  | ObjectGetter String
  -- |
  -- An record property accessor expression
  --
  | Accessor String Expr
  -- |
  -- Partial record update
  --
  | ObjectUpdate Expr [(String, Expr)]
  -- |
  -- Partial record updater. This will be removed during desugaring and
  -- expanded into a lambda that returns an object update.
  --
  | ObjectUpdater (Maybe Expr) [(String, Maybe Expr)]
  -- |
  -- Function introduction
  --
  | Abs (Either Ident Binder) Expr
  -- |
  -- Function application
  --
  | App Expr Expr
  -- |
  -- Variable
  --
  | Var (Qualified Ident)
  -- |
  -- Conditional (if-then-else expression)
  --
  | IfThenElse Expr Expr Expr
  -- |
  -- A data constructor
  --
  | Constructor (Qualified ProperName)
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case [Expr] [CaseAlternative]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Bool Expr Type
  -- |
  -- A let binding
  --
  | Let [Declaration] Expr
  -- |
  -- A do-notation block
  --
  | Do [DoNotationElement]
  -- |
  -- An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  --
  | TypeClassDictionaryConstructorApp (Qualified ProperName) Expr
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary Bool Constraint [TypeClassDictionaryInScope]
  -- |
  -- A typeclass dictionary accessor, the implementation is left unspecified until CoreFn desugaring.
  --
  | TypeClassDictionaryAccessor (Qualified ProperName) Ident
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | SuperClassDictionary (Qualified ProperName) [Type]
  -- |
  -- A value with source position information
  --
  | PositionedValue SourceSpan [Comment] Expr deriving (Show, D.Data, D.Typeable)

-- |
-- An alternative in a case statement
--
data CaseAlternative = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: Either [(Guard, Expr)] Expr
  } deriving (Show, D.Data, D.Typeable)

-- |
-- A statement in a do-notation block
--
data DoNotationElement
  -- |
  -- A monadic value without a binder
  --
  = DoNotationValue Expr
  -- |
  -- A monadic value with a binder
  --
  | DoNotationBind Binder Expr
  -- |
  -- A let statement, i.e. a pure value with a binder
  --
  | DoNotationLet [Declaration]
  -- |
  -- A do notation element with source position information
  --
  | PositionedDoNotationElement SourceSpan [Comment] DoNotationElement deriving (Show, D.Data, D.Typeable)
