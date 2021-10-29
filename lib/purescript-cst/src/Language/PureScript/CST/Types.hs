-- | This module contains data types for the entire PureScript surface language. Every
-- token is represented in the tree, and every token is annotated with
-- whitespace and comments (both leading and trailing). This means one can write
-- an exact printer so that `print . parse = id`. Every constructor is laid out
-- with tokens in left-to-right order. The core productions are given a slot for
-- arbitrary annotations, however this is not used by the parser.

module Language.PureScript.CST.Types where

import Prelude

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Language.PureScript.Names as N
import qualified Language.PureScript.Roles as R
import Language.PureScript.PSString (PSString)

-- |
-- Source code's line and column for a token.
data SourcePos = SourcePos
  { srcLine :: {-# UNPACK #-} !Int
  , srcColumn :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord, Generic)

-- |
-- Source code's start and end range for a token.
data SourceRange = SourceRange
  { srcStart :: !SourcePos
  , srcEnd :: !SourcePos
  } deriving (Show, Eq, Ord, Generic)

-- |
-- Non-code related content in the source code.
-- The @l@ type parameter is either `Void`, making the @Line@ data constructor imposible for that usage,
-- or `LineFeed`.
data Comment l
  = Comment !Text
  | Space {-# UNPACK #-} !Int
  | Line !l
  deriving (Show, Eq, Ord, Generic, Functor)

data LineFeed = LF | CRLF
  deriving (Show, Eq, Ord, Generic)

-- |
-- The position of a token in source code as well as
-- its surrounding comments (if any)
data TokenAnn = TokenAnn
  { tokRange :: !SourceRange
  , tokLeadingComments :: ![Comment LineFeed]
  , tokTrailingComments :: ![Comment Void]
  } deriving (Show, Eq, Ord, Generic)

-- |
-- Used to indicate whether a given keyword (e.g. "forall")
-- is using the text or unicode symbol version.
data SourceStyle = ASCII | Unicode
  deriving (Show, Eq, Ord, Generic)

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow !SourceStyle
  | TokRightArrow !SourceStyle
  | TokRightFatArrow !SourceStyle
  | TokDoubleColon !SourceStyle
  | TokForall !SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokLowerName ![Text] !Text
  | TokUpperName ![Text] !Text
  | TokOperator ![Text] !Text
  | TokSymbolName ![Text] !Text
  | TokSymbolArr !SourceStyle
  | TokHole !Text
  | TokChar !Text !Char
  | TokString !Text !PSString
  | TokRawString !Text
  | TokInt !Text !Integer
  | TokNumber !Text !Double
  | TokLayoutStart
    -- ^ Indicates the start of a new indentation block.
    -- All expressions must be indented at least to here to be considered part of the block.
  | TokLayoutSep
    -- ^ Indicates a separator between two entities within the same indentation block.
  | TokLayoutEnd
    -- ^ Indicates the end of an indentation block.
    -- All expressions after this one will be part of the previous block.
  | TokEof
  deriving (Show, Eq, Ord, Generic)

-- |
-- Source code token and annotation (i.e. position and comments)
data SourceToken = SourceToken
  { tokAnn :: !TokenAnn
  , tokValue :: !Token
  } deriving (Show, Eq, Ord, Generic)

-- |
-- An identifier as it appears in the source code. For example
-- - the @x@ in @functionName arg1 x = arg1@
data Ident = Ident
  { getIdent :: Text
  } deriving (Show, Eq, Ord, Generic)

data Name a = Name
  { nameTok :: SourceToken
  , nameValue :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A name (e.g. @name@) that may have a qualified module preceding it (e.g. @ModuleAlias.name@).
data QualifiedName a = QualifiedName
  { qualTok :: SourceToken
  , qualModule :: Maybe N.ModuleName
  , qualName :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a label in a row
data Label = Label
  { lblTok :: SourceToken
  , lblName :: PSString
  } deriving (Show, Eq, Ord, Generic)

-- |
-- An @a@ wrapped by an opening and closing boundary token (e.g. parenthesis)
data Wrapped a = Wrapped
  { wrpOpen :: SourceToken
  , wrpValue :: a
  , wrpClose :: SourceToken
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A non-empty list of @a@ where each @a@ is separated by some delimiter `SourceToken`.
data Separated a = Separated
  { sepHead :: a
  , sepTail :: [(SourceToken, a)]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Stores two values separated by @::@.
data Labeled a b = Labeled
  { lblLabel :: a
  , lblSep :: SourceToken
  , lblValue  :: b
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A potentially empty list of @a@ that is wrapped in some opening and closing boundary
-- `SourceToken` (e.g. parenthesis). Each @a@ element is separated by some delimiter `SourceToken`.
type Delimited a = Wrapped (Maybe (Separated a))

-- |
-- A non-empty list of @a@ that is wrapped in some opening and closing boundary
-- `SourceToken` (e.g. parenthesis). Each @a@ element is separated by some delimiter `SourceToken.
type DelimitedNonEmpty a = Wrapped (Separated a)

-- |
-- A single @a@ or a non-empty list of @a@ where each @a@ is separated by some delimiter `SourceToken`
-- that is wrapped in some opening and closing boundary `SourceToken` (e.g. parenthesis).
data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- The main @Type@ type. The @a@ type typically refers to the token's annotation
-- (e.g. position and comments) and always appears as the first argument for each constructor.
data Type a
  -- |
  -- An identifier in the source code, such as
  -- - The @x@ in @let x = 4 in ...@
  -- - The @x@ in @foo :: { x :: String }@
  -- - The @x@ in @foreign import foo :: { x :: String }@
  -- - The @x@ in @case y of Something x -> ...@
  -- - The @x@ in @class Bar x where ...@
  -- - The @x@ in @instance x :: Bar String where ...@
  -- - The @x@ in @module Foo (x) where ...@
  -- - The @x@ in @module Foo where import Bar (x) ...@
  = TypeVar a (Name Ident)
  -- |
  -- The name of a type (e.g. @Maybe@ or @ModuleAlias.Maybe@)
  | TypeConstructor a (QualifiedName (N.ProperName 'N.TypeName))
  -- |
  -- The @_@ character in
  -- > x :: Array _
  -- > x = [ 1 ]
  | TypeWildcard a SourceToken
  -- |
  -- The @?Help@ in
  -- > x :: Array ?Help
  -- > x = [ 1 ]
  | TypeHole a (Name Ident)
  -- |
  -- A Symbol (i.e. type-level String) written as a literal string
  | TypeString a SourceToken PSString
  -- |
  -- A few possible options
  -- - an empty row: @()@
  -- - an open row: @( | otherRows )@
  -- - a closed row with labels: @( label :: Type, label2 :: Type2 )@
  -- - an open row with labels: @( label :: Type | otherRows )@
  | TypeRow a (Wrapped (Row a))
  -- |
  -- A literal record type, whether open or closed:
  -- - an empty record: @{}@
  -- - an open record: @{ | otherRows @
  -- - a closed record with labels: @{ label :: Type, label2 :: Type2 }@
  -- - an open record with labels: @{ label :: Type | otherRows }@
  | TypeRecord a (Wrapped (Row a))
  -- |
  -- Given...
  -- > x :: forall a b c. a -> b -> c -> String
  -- > x _ _ _ = "foo"
  -- Arguments would be:
  -- - SourceToken: @forall@
  -- - NonEmpty (TypeVarBinding a): @a b c@
  -- - SourceToken: @.@
  -- - Type a - @a -> b -> c -> String@
  | TypeForall a SourceToken {- ^ The 'forall' keyword -} (NonEmpty (TypeVarBinding a)) SourceToken (Type a)
  -- |
  -- A type that is annotated with a kind. For example,
  -- > value :: Type
  -- Due to clashing with row syntax, this data constructor will often
  -- have its first type argument wrapped in a @TypeParens@
  -- For example, @(a :: Foo)@ is a row whereas @((a) :: Foo)@ is @a@ annotated with kind, @Foo@.
  | TypeKinded a (Type a) SourceToken (Type a)
  -- |
  -- Constructs a type via type application
  -- @Maybe String@ would be encoded as @TypeApp sourceAnnotation maybeType stringType@
  | TypeApp a (Type a) (Type a)
  -- |
  -- A potentially qualified operator (e.g. a sequence of symbols) in an infix position.
  -- For example, @Type1 %%% Type2@
  | TypeOp a (Type a) (QualifiedName (N.OpName 'N.TypeOpName)) (Type a)
  -- A potentially qualified operator (e.g. a sequence of symbols) in a prefix position.
  -- For example, @(%%%) Type1 Type2@
  | TypeOpName a (QualifiedName (N.OpName 'N.TypeOpName))
  -- |
  -- The first type for a Function in its infix position.
  -- For example, @InputType -> OutputType@
  | TypeArr a (Type a) SourceToken (Type a)
  -- |
  -- The second type for a Function in its prefix position.
  -- For example, the @(->)@ in @(->) InputType OutputType@
  | TypeArrName a SourceToken
  -- |
  -- The @(Show a, Eq a) => ShowEq (f a)@ in
  -- > instance instName :: (Show a, Eq a) => ShowEq (f a) where ...
  | TypeConstrained a (Constraint a) SourceToken (Type a)
  -- |
  -- Wraps the type in parenthesis source tokens
  | TypeParens a (Wrapped (Type a))
  -- |
  -- Deprecated row syntax via the @#@ character
  -- The kind, @Row Type@, was previously @# Type@
  | TypeUnaryRow a SourceToken (Type a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents the identifier used for a type variable as found in the following
-- non-exhaustive examples:
-- - @functionName :: forall typeVarBinding. InputType -> OutputType@
-- - @data TypeName typeVarBinding = TypeName@
data TypeVarBinding a
  -- |
  -- A type variable that has a kind annotation. For example
  -- @data TypeName (a :: Symbol) = TypeName@
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (Type a)))
  -- |
  -- A type variable that does not have a kind annotation.
  | TypeVarName (Name Ident)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Constraint a
  = Constraint a (QualifiedName (N.ProperName 'N.ClassName)) [Type a]
  | ConstraintParens a (Wrapped (Constraint a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a row without the surrounding boundary characters
-- (e.g. parenthesis if row, curly braces if record):
-- - @@ -- empty
-- - @label1 :: Type1, label2 :: Type2@ -- has labels, but no open tail
-- - @| allOtherRows@ -- no labels, but has open tail
-- - @label1 :: Type1, label2 :: Type2 | allOtherRows@ -- has labels and open tail
data Row a = Row
  { rowLabels :: Maybe (Separated (Labeled Label (Type a)))
  , rowTail :: Maybe (SourceToken, Type a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a CST module
data Module a = Module
  { modAnn :: a
  , modKeyword :: SourceToken
  , modNamespace :: Name N.ModuleName
  , modExports :: Maybe (DelimitedNonEmpty (Export a))
  , modWhere :: SourceToken
  , modImports :: [ImportDecl a]
  , modDecls :: [Declaration a]
  , modTrailingComments :: [Comment LineFeed]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a CST export
data Export a
  -- |
  -- Exports a value. For example
  -- @
  -- module Main (value) where
  --
  -- value :: String
  -- value = "exported value"
  -- @
  = ExportValue a (Name Ident)
  -- |
  -- Exports a value-level symbolic operator. For example
  -- @
  -- module Main ((++++)) where
  --
  -- value :: String
  -- value = "exported value"
  --
  -- infixl 4 value as ++++
  -- @
  | ExportOp a (Name (N.OpName 'N.ValueOpName))
  -- |
  -- Exports a Type and potentially its members. For example
  -- @
  -- module Main (Type(..)) where
  --
  -- data Type = Constructor
  -- @
  -- or...
  -- @
  -- module Main (Type) where
  --
  -- data Type = Constructor
  -- @
  | ExportType a (Name (N.ProperName 'N.TypeName)) (Maybe (DataMembers a))
  -- |
  -- Exports a type-level symbolic operator. For example
  -- @
  -- module Main ((++++)) where
  --
  -- data RowApply f a = f a
  --
  -- infixl 4 type RowApply as ++++
  -- @
  | ExportTypeOp a SourceToken (Name (N.OpName 'N.TypeOpName))
  -- |
  -- Exports a type class. For example
  -- @
  -- module Main (class TypeClassName) where
  --
  -- class TypeClassName
  -- @
  | ExportClass a SourceToken (Name (N.ProperName 'N.ClassName))
  -- |
  -- Deprecated syntax for exporting a kind. Will be removed in next breaking change. For example
  -- @
  -- module Main (kind MyKind) where
  --
  -- foreign import data MyKind :: Type
  -- @
  | ExportKind a SourceToken (Name (N.ProperName 'N.TypeName))
  -- |
  -- Syntax for exporting a module. For example.
  -- @
  -- module Main (module ModuleName) where
  --
  -- import Module (a, className, class Foo) as ModuleName
  -- @
  | ExportModule a SourceToken (Name N.ModuleName)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Indicates whether all, some, or none of a type's members are exported.
data DataMembers a
  -- |
  -- All of the type's members are exported
  = DataAll a SourceToken
  -- |
  -- Some or none of the type's members are exported
  | DataEnumerated a (Delimited (Name (N.ProperName 'N.ConstructorName)))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a CST declaration, or everything that comes after a module's imports section
-- that is not a comment.
data Declaration a
  -- |
  -- Represents a data type declaration:
  -- @
  -- data MyType possibleTypeVariables
  --   = DataConstructor1
  --   | DataConstructor2 WithArguments
  -- @
  = DeclData a (DataHead a) (Maybe (SourceToken, Separated (DataCtor a)))
  -- |
  -- Represents a type alias declaration:
  -- @
  -- type MyType possibleTypeVariables = Int
  -- @
  | DeclType a (DataHead a) SourceToken (Type a)
  -- |
  -- Represents a newtype declaration:
  -- @
  -- newtype MyType possibleTypeVariables = DataConstructor ActualType
  -- @
  | DeclNewtype a (DataHead a) SourceToken (Name (N.ProperName 'N.ConstructorName)) (Type a)
  -- |
  -- Represents a type class declaration:
  -- @
  -- class ClassName possibleTypeVariables where
  --   possibleMember :: possibleTypeVariables -> String
  -- @
  | DeclClass a (ClassHead a) (Maybe (SourceToken, NonEmpty (Labeled (Name Ident) (Type a))))
  -- |
  -- Represents a type class instance declaration:
  -- @
  -- instance possibleInstanceName :: ClassName TypeName where
  --   possibleMember :: possibleTypeVariables -> String
  --   possibleMember _ = "foo"
  -- else
  -- instance ClassName Bar where
  --   posibleMember _ = "bar"
  -- @
  | DeclInstanceChain a (Separated (Instance a))
  -- |
  -- Represents a derived type class instance declaration:
  --
  -- @derive instance possibleInstanceName :: Newtype NewtypeName _@
  --
  -- or
  --
  -- @derive newtype instance possibleInstanceName :: Show NewtypeName@
  | DeclDerive a SourceToken (Maybe SourceToken) (InstanceHead a)
  -- |
  -- Represents a kind signature declaration:
  --
  -- @
  -- data Type :: forall k. k -> Type
  -- @
  --
  -- or similarly for @type@ or @newtype@.
  | DeclKindSignature a SourceToken (Labeled (Name (N.ProperName 'N.TypeName)) (Type a))
  -- |
  -- Represents a type signature declaration:
  --
  -- @
  -- functionName :: forall a. a -> String
  -- @
  | DeclSignature a (Labeled (Name Ident) (Type a))
  -- |
  -- Represents a function or value declaration
  -- (i.e. the part under a function/value's type signature):
  --
  -- @
  -- function arg1 arg2 _ | true || (arg1 == arg2) = doSomthing
  -- @
  --
  -- or
  --
  -- @
  -- value = "foo"
  -- @
  | DeclValue a (ValueBindingFields a)
  -- |
  -- Represents an infix declaration.
  --
  -- @
  -- infix 4 functionName as ###
  -- @
  | DeclFixity a FixityFields
  -- |
  -- Represents a foreign function interface (FFI) value, function, or data type declaration.
  --
  -- @
  -- -- A couple of examples:
  -- foreign import functionName :: Int -> String
  -- foreign import valueName :: Int
  -- foreign import data TypeName :: Int -> String
  -- @
  | DeclForeign a SourceToken SourceToken (Foreign a)
  -- |
  -- Represents a role declaration
  --
  -- @
  -- type role SomeType phantom representational nomainl
  -- @
  | DeclRole a SourceToken SourceToken (Name (N.ProperName 'N.TypeName)) (NonEmpty Role)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A single type class instance
data Instance a = Instance
  { instHead :: InstanceHead a -- ^ Everything before the @where@ keyword
  , instBody :: Maybe (SourceToken, NonEmpty (InstanceBinding a)) -- ^ the @where@ keyword followed by the body
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Either the type signature declaration for a type class member
-- (e.g. @Functor@'s @map :: forall a b. (a -> b) -> f a -> f b@)
-- or a member's implementation.
data InstanceBinding a
  -- |
  -- The type signature declaration for a type class member
  -- (e.g. @Functor@'s @map :: forall a b. (a -> b) -> f a -> f b@)
  = InstanceBindingSignature a (Labeled (Name Ident) (Type a))
  -- |
  -- The implementation for a type class member
  -- (e.g. @map (Identity a) f = Identity (f a)@).
  | InstanceBindingName a (ValueBindingFields a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A module's imports
data ImportDecl a = ImportDecl
  { impAnn :: a
  , impKeyword :: SourceToken
  , impModule :: Name N.ModuleName
  , impNames :: Maybe (Maybe SourceToken, DelimitedNonEmpty (Import a))
    -- ^ if a @Just@, then a potential @hiding@ keyword, followed by the imported names of the module's members
  , impQual :: Maybe (SourceToken, Name N.ModuleName)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- The type of the import declaration
data Import a
  -- |
  -- Imports a value or function
  = ImportValue a (Name Ident)
  -- |
  -- Imports a value-level symbolic operator
  | ImportOp a (Name (N.OpName 'N.ValueOpName))
  -- |
  -- Imports a data type or newtype and potentially its constructor(s)
  | ImportType a (Name (N.ProperName 'N.TypeName)) (Maybe (DataMembers a))
  -- |
  -- Imports a type alias
  | ImportTypeOp a SourceToken (Name (N.OpName 'N.TypeOpName))
  -- |
  -- Imports a type class
  | ImportClass a SourceToken (Name (N.ProperName 'N.ClassName))
  -- |
  -- Deprecated syntax for importing kinds. This will be removed in a future breaking release.
  | ImportKind a SourceToken (Name (N.ProperName 'N.TypeName))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Everything for a data type, newtype, or type alias that appears on the left side of the @=@ character
data DataHead a = DataHead
  { dataHdKeyword :: SourceToken
  , dataHdName :: Name (N.ProperName 'N.TypeName)
  , dataHdVars :: [TypeVarBinding a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents a data constructor (i.e. what appears on the right of the @=@ character)
data DataCtor a = DataCtor
  { dataCtorAnn :: a
  , dataCtorName :: Name (N.ProperName 'N.ConstructorName)
  , dataCtorFields :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents the head of a type class (i.e. everything after the @class@ keyword
-- and the part that goes under a type class kind signature).
data ClassHead a = ClassHead
  { clsKeyword :: SourceToken
  , clsSuper :: Maybe (OneOrDelimited (Constraint a), SourceToken)
  , clsName :: Name (N.ProperName 'N.ClassName)
  , clsVars :: [TypeVarBinding a]
  , clsFundeps :: Maybe (SourceToken, Separated ClassFundep)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Indicates the type of functional dependency in a type class
data ClassFundep
  -- |
  -- @-> tyVar3@
  = FundepDetermined SourceToken (NonEmpty (Name Ident))
  -- |
  -- @tyVar1 tyVar2 -> tyVar3@
  | FundepDetermines (NonEmpty (Name Ident)) SourceToken (NonEmpty (Name Ident))
  deriving (Show, Eq, Ord, Generic)

-- |
-- Represents the head of a type class instance (e.g. everything before the @where@ keyword)
data InstanceHead a = InstanceHead
  { instKeyword :: SourceToken
  , instNameSep :: Maybe (Name Ident, SourceToken)
  , instConstraints :: Maybe (OneOrDelimited (Constraint a), SourceToken)
  , instClass :: QualifiedName (N.ProperName 'N.ClassName)
  , instTypes :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- The association direction of an symbolic operator (if any).
data Fixity
  -- |
  -- @ "a" $$$$$ "b"  $$$$$  "c" $$$$$ "d"@
  --
  -- becomes
  --
  -- @("a" $$$$$ "b") $$$$$ ("c" $$$$$ "d")@
  = Infix
  -- |
  -- @  "a" |>>| "b"  |>>| "c"  |>>| "d"@
  --
  -- becomes
  --
  -- @(("a" |>>| "b") |>>| "c") |>>| "d"@
  | Infixl
  -- |
  -- @"a" |<<|  "b" |<<|  "c" |<<| "d"@
  --
  -- becomes
  --
  -- @"a" |<<| ("b" |<<| ("c" |<<| "d"))@
  | Infixr
  deriving (Show, Eq, Ord, Generic)

-- |
-- Defines whether the symbolic operator is an alias for a value-level identifier
-- or a type-level identifier
data FixityOp
  -- |
  -- The @valueName as ***@ part in @infixl 4 valueName as ***@
  = FixityValue (QualifiedName (Either Ident (N.ProperName 'N.ConstructorName))) SourceToken (Name (N.OpName 'N.ValueOpName))
  -- |
  -- The @TypeName as ***@ part in @infixl 4 type TypeName as ***@
  | FixityType SourceToken (QualifiedName (N.ProperName 'N.TypeName)) SourceToken (Name (N.OpName 'N.TypeOpName))
  deriving (Show, Eq, Ord, Generic)

data FixityFields = FixityFields
  { fxtKeyword :: (SourceToken, Fixity) -- ^ infixl, infixr, or infix keyword
  , fxtPrec :: (SourceToken, Integer) -- ^ the infix precendence level
  , fxtOp :: FixityOp -- ^ the infix type (i.e. value or type)
  } deriving (Show, Eq, Ord, Generic)

-- |
-- Represents a value or a function's implementation
-- (i.e. the part that appears under the type signature).
--
-- @
-- let x arg1 arg2
--   | guard arg1 = arg2
--   | otherwise = arg1
-- @
data ValueBindingFields a = ValueBindingFields
  { valName :: Name Ident
  , valBinders :: [Binder a]
  , valGuarded :: Guarded a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Indicates whether a guard exists in a function or value's implementation
data Guarded a
  -- |
  -- No guards are used
  = Unconditional SourceToken (Where a)
  -- |
  -- One or more guards are used
  | Guarded (NonEmpty (GuardedExpr a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data GuardedExpr a = GuardedExpr
  { grdBar :: SourceToken
  , grdPatterns :: Separated (PatternGuard a)
  , grdSep :: SourceToken
  , grdWhere :: Where a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data PatternGuard a = PatternGuard
  { patBinder :: Maybe (Binder a, SourceToken)
  , patExpr :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- The content after the @foreign import@ syntax. For example,
-- @valueName :: String@ in @foreign import valueName :: String@.
data Foreign a
  -- |
  -- A foreign value, such as @foreign import value :: String@
  = ForeignValue (Labeled (Name Ident) (Type a))
  -- |
  -- A foreign data type, such as @foreign import data TypeName :: Type -> Type@
  | ForeignData SourceToken (Labeled (Name (N.ProperName 'N.TypeName)) (Type a))
  -- |
  -- Deprecated syntax for defining kinds. This will be removed in a future breaking release.
  --
  -- A kind defintion, such as @foreign import kind KindName@
  | ForeignKind SourceToken (Name (N.ProperName 'N.TypeName))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Role = Role
  { roleTok :: SourceToken
  , roleValue :: R.Role
  } deriving (Show, Eq, Ord, Generic)

-- |
-- A value-level term in the language.
data Expr a
  -- |
  -- The @?WhatIsThis@ text in
  --
  -- @
  -- myVal :: String
  -- myVal = ?WhatIsThis
  -- @
  = ExprHole a (Name Ident)
  -- |
  -- The @_@ text in
  --
  -- @
  -- myVal :: String
  -- myVal = _
  -- @
  | ExprSection a SourceToken
  -- |
  -- The second @name@ text in
  --
  -- @
  -- myVal :: String -> String
  -- myVal name = name
  -- @
  | ExprIdent a (QualifiedName Ident)
  | ExprConstructor a (QualifiedName (N.ProperName 'N.ConstructorName))
  -- | A literal @true@ or @false@ value
  | ExprBoolean a SourceToken Bool
  -- | A literal @Char@ value
  | ExprChar a SourceToken Char
  -- | A literal @String@ value
  | ExprString a SourceToken PSString
  -- | A literal @Int@ or @Number@ value
  | ExprNumber a SourceToken (Either Integer Double)
  -- | A literal @Array@ value
  | ExprArray a (Delimited (Expr a))
  -- | A literal @Record@ value
  | ExprRecord a (Delimited (RecordLabeled (Expr a)))
  -- | An expression wrapped in parenthesis
  | ExprParens a (Wrapped (Expr a))
  -- | An expression followed by a type annotation (e.g. @expr :: Type@)
  | ExprTyped a (Expr a) SourceToken (Type a)
  -- | An infixed version of a function call (e.g. @arg1 `functionName` arg2).
  | ExprInfix a (Expr a) (Wrapped (Expr a)) (Expr a)
  -- | An infixed version of a symbolic operator (e.g. @1 + 2@)
  | ExprOp a (Expr a) (QualifiedName (N.OpName 'N.ValueOpName)) (Expr a)
  -- | A standalone version of a symbolic operator (e.g. @(+)@ in @fold (+) 0 [1]@)
  | ExprOpName a (QualifiedName (N.OpName 'N.ValueOpName))
  -- | Negate a numerical value
  | ExprNegate a SourceToken (Expr a)
  -- | Access a field within a record (e.g. @record.field@)
  | ExprRecordAccessor a (RecordAccessor a)
  -- | Update a field within a record with a new value (e.g. @record { field = new }@)
  | ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  -- | Apply an argument to a function (e.g. @(\x -> x) 4@)
  | ExprApp a (Expr a) (Expr a)
  -- | Define an anonymous function (e.g. @\x -> x + 1@)
  | ExprLambda a (Lambda a)
  -- | An @if cond then truePath else falsePath@ expression
  | ExprIf a (IfThenElse a)
  -- | A @case b1, b2, ..., bn of@ expression
  | ExprCase a (CaseOf a)
  -- | A @let ... in ...@ expression
  | ExprLet a (LetIn a)
  -- | A monadic do expression
  | ExprDo a (DoBlock a)
  -- | An applicative do expression
  | ExprAdo a (AdoBlock a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField Label SourceToken a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordUpdate a
  = RecordUpdateLeaf Label SourceToken (Expr a)
  | RecordUpdateBranch Label (DelimitedNonEmpty (RecordUpdate a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordAccessor a = RecordAccessor
  { recExpr :: Expr a
  , recDot :: SourceToken
  , recPath :: Separated Label
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Lambda a = Lambda
  { lmbSymbol :: SourceToken
  , lmbBinders :: NonEmpty (Binder a)
  , lmbArr :: SourceToken
  , lmbBody :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data IfThenElse a = IfThenElse
  { iteIf :: SourceToken
  , iteCond :: Expr a
  , iteThen :: SourceToken
  , iteTrue :: Expr a
  , iteElse :: SourceToken
  , iteFalse :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data CaseOf a = CaseOf
  { caseKeyword :: SourceToken
  , caseHead :: Separated (Expr a)
  , caseOf :: SourceToken
  , caseBranches :: NonEmpty (Separated (Binder a), Guarded a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data LetIn a = LetIn
  { letKeyword :: SourceToken
  , letBindings :: NonEmpty (LetBinding a)
  , letIn :: SourceToken
  , letBody :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- The body of a function, let binding, etc., which may have a @where@ block
-- that follows that body.
data Where a = Where
  { whereExpr :: Expr a
  , whereBindings :: Maybe (SourceToken, NonEmpty (LetBinding a))
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data LetBinding a
  -- | The type signature for a let binding (e.g. @let x :: String@)
  = LetBindingSignature a (Labeled (Name Ident) (Type a))
  -- | The implementation for a let binding (e.g. @let x arg1 arg2 = ...)
  | LetBindingName a (ValueBindingFields a)
  -- | The implementation for a let binding (e.g. @let x arg1 arg2 = ...)
  | LetBindingPattern a (Binder a) SourceToken (Where a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A monadic do block
--
-- @
--    do
--  let value = 1
--  x <- foo
--  bar
-- @
data DoBlock a = DoBlock
  { doKeyword :: SourceToken
  , doStatements :: NonEmpty (DoStatement a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- One bind expression in a monadic do block
--
-- @
--    do
--  let          -- DoLet
--    value1 = 1
--    value2 = 1
--  x <- foo     -- DoBind
--  bar          -- DoDiscard
-- @
data DoStatement a
  -- |
  -- @
  --    do
  --  let
  --    value1 = 1
  --    value2 = 1
  -- @
  = DoLet SourceToken (NonEmpty (LetBinding a))
  -- |
  -- @
  --    do
  --  x <- foo
  -- @
  | DoDiscard (Expr a)
  -- |
  -- @
  --    do
  --  bar
  -- @
  | DoBind (Binder a) SourceToken (Expr a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- One bind expression in an applicative do block
--
-- @
--    ado
--  let          -- DoLet
--    value1 = 1
--    value2 = 1
--  x <- foo     -- DoBind
--  bar          -- DoDiscard
--  in x + value1 + value2
-- @
data AdoBlock a = AdoBlock
  { adoKeyword :: SourceToken
  , adoStatements :: [DoStatement a]
  , adoIn :: SourceToken
  , adoResult :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Syntax for pattern matching (e.g. "case _ of" branches, let bindings, etc.)
data Binder a
  -- | Binder for @_@, which ignores the argument completely.
  = BinderWildcard a SourceToken
  -- | Binds the given name to a value (e.g. @someNameInCaseBranch -> ...@)
  | BinderVar a (Name Ident)
  -- | Binds the given name to a value that may have additional binders
  -- (e.g. @name\@{ literal: "record" }@)
  | BinderNamed a (Name Ident) SourceToken (Binder a)
  -- | Binds the given constructor
  | BinderConstructor a (QualifiedName (N.ProperName 'N.ConstructorName)) [Binder a]
  -- | Binds to a literal true or false value
  | BinderBoolean a SourceToken Bool
  -- | Binds to a literal char value
  | BinderChar a SourceToken Char
  -- | Binds to a literal String value
  | BinderString a SourceToken PSString
  -- | Binds to a literal numerical value that may have a @-@ character in front of it
  | BinderNumber a (Maybe SourceToken) SourceToken (Either Integer Double)
  -- | Binds to a literal array value
  | BinderArray a (Delimited (Binder a))
  -- | Binds to a literal record value
  | BinderRecord a (Delimited (RecordLabeled (Binder a)))
  -- | Binds to a value wrapped in parenthesis
  | BinderParens a (Wrapped (Binder a))
  -- | Binds to a value with a type annotation (e.g. @(foo :: Int) <- ...@)
  | BinderTyped a (Binder a) SourceToken (Type a)
  -- | Binds to a symbolic operator (e.g. @(value1 :| value2)@)
  | BinderOp a (Binder a) (QualifiedName (N.OpName 'N.ValueOpName)) (Binder a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
