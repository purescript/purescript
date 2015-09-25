-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Declarations
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Data types for modules and declarations
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.AST.Declarations where

import Data.Aeson.TH

import qualified Data.Data as D
import qualified Data.Map as M

import Control.Monad.Identity

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Operators
import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Comments
import Language.PureScript.Environment

-- |
-- A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
--
data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef]) deriving (Show, Read, D.Data, D.Typeable)

-- | Return a module's name.
getModuleName :: Module -> ModuleName
getModuleName (Module _ _ name _ _) = name

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
  -- A module, in its entirety
  --
  | ModuleRef ModuleName
  -- |
  -- A declaration reference with source position information
  --
  | PositionedDeclarationRef SourceSpan [Comment] DeclarationRef
  deriving (Show, Read, D.Data, D.Typeable)

instance Eq DeclarationRef where
  (TypeRef name dctors)  == (TypeRef name' dctors') = name == name' && dctors == dctors'
  (ValueRef name)        == (ValueRef name')        = name == name'
  (TypeClassRef name)    == (TypeClassRef name')    = name == name'
  (TypeInstanceRef name) == (TypeInstanceRef name') = name == name'
  (ModuleRef name) == (ModuleRef name') = name == name'
  (PositionedDeclarationRef _ _ r) == r' = r == r'
  r == (PositionedDeclarationRef _ _ r') = r == r'
  _ == _ = False

isModuleRef :: DeclarationRef -> Bool
isModuleRef (ModuleRef _) = True
isModuleRef _ = False

-- |
-- The data type which specifies type of import declaration
--
data ImportDeclarationType
  -- |
  -- An import with no explicit list: `import M`
  --
  = Implicit
  -- |
  -- An import with an explicit list of references to import: `import M (foo)`
  --
  | Explicit [DeclarationRef]
  -- |
  -- An import with a list of references to hide: `import M hiding (foo)`
  --
  | Hiding [DeclarationRef]
  deriving (Show, Read, D.Data, D.Typeable)

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
  -- A foreign import declaration (name, type)
  --
  | ExternDeclaration Ident Type
  -- |
  -- A data type foreign import (name, kind)
  --
  | ExternDataDeclaration ProperName Kind
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
  | TypeInstanceDeclaration Ident [Constraint] (Qualified ProperName) [Type] TypeInstanceBody
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration SourceSpan [Comment] Declaration
  deriving (Show, Read, D.Data, D.Typeable)

-- | The members of a type class instance declaration
data TypeInstanceBody
  -- | This is a derived instance
  = DerivedInstance
  -- | This is a regular (explicit) instance
  | ExplicitInstance [Declaration]
  deriving (Show, Read, D.Data, D.Typeable)

mapTypeInstanceBody :: ([Declaration] -> [Declaration]) -> TypeInstanceBody -> TypeInstanceBody
mapTypeInstanceBody f = runIdentity . traverseTypeInstanceBody (Identity . f)

-- | A traversal for TypeInstanceBody
traverseTypeInstanceBody :: (Applicative f) => ([Declaration] -> f [Declaration]) -> TypeInstanceBody -> f TypeInstanceBody
traverseTypeInstanceBody _ DerivedInstance = pure DerivedInstance
traverseTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance <$> f ds

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
  -- A character literal
  --
  | CharLiteral Char
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
  | TypeClassDictionary Constraint (M.Map (Maybe ModuleName) (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope)))
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
  | PositionedValue SourceSpan [Comment] Expr deriving (Show, Read, D.Data, D.Typeable)

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
  } deriving (Show, Read, D.Data, D.Typeable)

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
  | PositionedDoNotationElement SourceSpan [Comment] DoNotationElement deriving (Show, Read, D.Data, D.Typeable)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''DeclarationRef)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ImportDeclarationType)
