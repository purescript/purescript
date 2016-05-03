{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Data types for modules and declarations
--
module Language.PureScript.AST.Declarations where

import Prelude ()
import Prelude.Compat

import Data.Aeson.TH
import Data.List (nub, (\\))
import Data.Maybe (mapMaybe)

import qualified Data.Map as M

import Control.Monad.Identity

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Literals
import Language.PureScript.AST.Operators
import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Comments
import Language.PureScript.Environment

-- | The module header consists of everything up to the end of
-- the import section.
--
-- This is all that is needed to determine the module graph.
data ModuleHeader = ModuleHeader
  { moduleHeaderName :: ModuleName
  , moduleHeaderExports :: Maybe [DeclarationRef]
  , moduleHeaderImports :: [Declaration]
  } deriving (Show, Read)

-- |
-- A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
--
data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])
  deriving (Show, Read)

-- | Extract a valid module header from an existing module.
--
-- Note, it is generally better to obtain a 'ModuleHeader' by parsing it separately,
-- since parsing is cheaper compared to a full 'Module'.
extractModuleHeader :: Module -> ModuleHeader
extractModuleHeader (Module _ _ name ds exps) = ModuleHeader name exps (filter isImportDecl ds)

-- | Return a module's name.
getModuleName :: Module -> ModuleName
getModuleName (Module _ _ name _ _) = name

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: ModuleName -> Module -> Module
addDefaultImport toImport m@(Module ss coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module ss coms mn (ImportDeclaration toImport Implicit Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

-- |
-- An item in a list of explicit imports or exports
--
data DeclarationRef
  -- |
  -- A type constructor with data constructors
  --
  = TypeRef (ProperName 'TypeName) (Maybe [ProperName 'ConstructorName])
  -- |
  -- A type operator
  --
  | TypeOpRef Ident
  -- |
  -- A value
  --
  | ValueRef Ident
  -- |
  -- A type class
  --
  | TypeClassRef (ProperName 'ClassName)
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
  deriving (Show, Read)

instance Eq DeclarationRef where
  (TypeRef name dctors)  == (TypeRef name' dctors') = name == name' && dctors == dctors'
  (TypeOpRef name)       == (TypeOpRef name')       = name == name'
  (ValueRef name)        == (ValueRef name')        = name == name'
  (TypeClassRef name)    == (TypeClassRef name')    = name == name'
  (TypeInstanceRef name) == (TypeInstanceRef name') = name == name'
  (ModuleRef name)       == (ModuleRef name')       = name == name'
  (PositionedDeclarationRef _ _ r) == r' = r == r'
  r == (PositionedDeclarationRef _ _ r') = r == r'
  _ == _ = False

isModuleRef :: DeclarationRef -> Bool
isModuleRef (PositionedDeclarationRef _ _ r) = isModuleRef r
isModuleRef (ModuleRef _) = True
isModuleRef _ = False

-- |
-- Finds duplicate values in a list of declaration refs. The returned values
-- are the duplicate refs with data constructors elided, and then a separate
-- list of duplicate data constructors.
--
findDuplicateRefs :: [DeclarationRef] -> ([DeclarationRef], [ProperName 'ConstructorName])
findDuplicateRefs refs =
  let positionless = stripPosInfo `map` refs
      simplified = simplifyTypeRefs `map` positionless
      dupeRefs = nub $ simplified \\ nub simplified
      dupeCtors = concat $ flip mapMaybe positionless $ \case
        TypeRef _ (Just dctors) ->
          let dupes = dctors \\ nub dctors
          in if null dupes then Nothing else Just dupes
        _ -> Nothing
  in (dupeRefs, dupeCtors)
  where
  stripPosInfo (PositionedDeclarationRef _ _ ref) = stripPosInfo ref
  stripPosInfo other = other
  simplifyTypeRefs (TypeRef pn _) = TypeRef pn Nothing
  simplifyTypeRefs other = other

-- |
-- The data type which specifies type of import declaration
--
data ImportDeclarationType
  -- |
  -- An import with no explicit list: `import M`.
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
  deriving (Eq, Show, Read)

isImplicit :: ImportDeclarationType -> Bool
isImplicit Implicit = True
isImplicit _ = False

isExplicit :: ImportDeclarationType -> Bool
isExplicit (Explicit _) = True
isExplicit _ = False

-- |
-- The data type of declarations
--
data Declaration
  -- |
  -- A data type declaration (data or newtype, name, arguments, data constructors)
  --
  = DataDeclaration DataDeclType (ProperName 'TypeName) [(String, Maybe Kind)] [(ProperName 'ConstructorName, [Type])]
  -- |
  -- A minimal mutually recursive set of data type declarations
  --
  | DataBindingGroupDeclaration [Declaration]
  -- |
  -- A type synonym declaration (name, arguments, type)
  --
  | TypeSynonymDeclaration (ProperName 'TypeName) [(String, Maybe Kind)] Type
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
  | ExternDataDeclaration (ProperName 'TypeName) Kind
  -- |
  -- A fixity declaration (fixity data, operator name, value the operator is an alias for)
  --
  | FixityDeclaration Fixity String (Maybe (Qualified FixityAlias))
  -- |
  -- A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
  --
  | ImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName)
  -- |
  -- A type class declaration (name, argument, implies, member declarations)
  --
  | TypeClassDeclaration (ProperName 'ClassName) [(String, Maybe Kind)] [Constraint] [Declaration]
  -- |
  -- A type instance declaration (name, dependencies, class name, instance types, member
  -- declarations)
  --
  | TypeInstanceDeclaration Ident [Constraint] (Qualified (ProperName 'ClassName)) [Type] TypeInstanceBody
  -- |
  -- A declaration with source position information
  --
  | PositionedDeclaration SourceSpan [Comment] Declaration
  deriving (Show, Read)

data FixityAlias
  = AliasValue Ident
  | AliasConstructor (ProperName 'ConstructorName)
  | AliasType (ProperName 'TypeName)
  deriving (Eq, Ord, Show, Read)

foldFixityAlias
  :: (Ident -> a)
  -> (ProperName 'ConstructorName -> a)
  -> (ProperName 'TypeName -> a)
  -> FixityAlias
  -> a
foldFixityAlias f _ _ (AliasValue name) = f name
foldFixityAlias _ g _ (AliasConstructor name) = g name
foldFixityAlias _ _ h (AliasType name) = h name

getValueAlias :: FixityAlias -> Maybe (Either Ident (ProperName 'ConstructorName))
getValueAlias (AliasValue name) = Just $ Left name
getValueAlias (AliasConstructor name) = Just $ Right name
getValueAlias _ = Nothing

getTypeAlias :: FixityAlias -> Maybe (ProperName 'TypeName)
getTypeAlias (AliasType name) = Just name
getTypeAlias _ = Nothing

-- | The members of a type class instance declaration
data TypeInstanceBody
  -- | This is a derived instance
  = DerivedInstance
  -- | This is a regular (explicit) instance
  | ExplicitInstance [Declaration]
  deriving (Show, Read)

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
  -- A literal value
  --
  = Literal (Literal Expr)
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
  -- Note: although it seems this constructor is not used, it _is_ useful, since it prevents
  -- certain traversals from matching.
  --
  | Parens Expr
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
  | Constructor (Qualified (ProperName 'ConstructorName))
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
  | TypeClassDictionaryConstructorApp (Qualified (ProperName 'ClassName)) Expr
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary Constraint (M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) TypeClassDictionaryInScope)))
  -- |
  -- A typeclass dictionary accessor, the implementation is left unspecified until CoreFn desugaring.
  --
  | TypeClassDictionaryAccessor (Qualified (ProperName 'ClassName)) Ident
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | SuperClassDictionary (Qualified (ProperName 'ClassName)) [Type]
  -- |
  -- A placeholder for an anonymous function argument
  --
  | AnonymousArgument
  -- |
  -- A typed hole that will be turned into a hint/error duing typechecking
  --
  | Hole String
  -- |
  -- A value with source position information
  --
  | PositionedValue SourceSpan [Comment] Expr
  deriving (Show, Read)

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
  } deriving (Show, Read)

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
  | PositionedDoNotationElement SourceSpan [Comment] DoNotationElement
  deriving (Show, Read)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''DeclarationRef)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ImportDeclarationType)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''FixityAlias)
