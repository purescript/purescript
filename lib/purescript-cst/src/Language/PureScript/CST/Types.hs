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
  { tokRange :: !SourceRange -- ^ The position of the token in source code
  , tokLeadingComments :: ![Comment LineFeed] -- ^ Comments before the token
  , tokTrailingComments :: ![Comment Void] -- ^ Comments after the token
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
  | TokLayoutSep
  | TokLayoutEnd
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
-- Conceptually, if the @a@ was @Int@ and the `SourceToken` was @Text@ and the below syntax was supported:
-- - @[1]@
-- - @[1, "a", 2]@
-- - @[1, "a", 2, "b", 3, "c", 4]@
data Separated a = Separated
  { sepHead :: a
  , sepTail :: [(SourceToken, a)]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- Represents @a :: b@, such as what is found in a row
-- (e.g. the @label :: String@ part in @( label :: String )@)
data Labeled a b = Labeled
  { lblLabel :: a
  , lblSep :: SourceToken
  , lblValue  :: b
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

-- |
-- A potentially empty list of @a@ that is wrapped in some opening and closing boundary
-- `SourceToken` (e.g. parenthesis). Each @a@ element is separated by some delimiter `SourceToken`.
--
-- Conceptually, if the @a@ was @Int@ and the `SourceToken` was @Text@ and the boundary
-- tokens were parenthesis and the below syntax was supported,
-- - @([])@
-- - @([1])@
-- - @([1, "a", 2])@
-- - @([1, "a", 2, "b", 3, "c", 4])@
type Delimited a = Wrapped (Maybe (Separated a))

-- |
-- A non-empty list of @a@ that is wrapped in some opening and closing boundary
-- `SourceToken` (e.g. parenthesis). Each @a@ element is separated by some delimiter `SourceToken.
--
-- Conceptually, if the @a@ was @Int@ and the `SourceToken` was @Text@ and the boundary
-- tokens were parenthesis and the below syntax was supported,
-- - @([1])@
-- - @([1, "a", 2])@
-- - @([1, "a", 2, "b", 3, "c", 4])@
type DelimitedNonEmpty a = Wrapped (Separated a)

-- |
-- A single @a@ or a non-empty list of @a@ where each @a@ is separated by some delimiter `SourceToken`
-- that is wrapped in some opening and closing boundary `SourceToken` (e.g. parenthesis).
--
-- Conceptually, if the @a@ was @Int@ and the `SourceToken` was @Text@ and the boundary
-- tokens were parenthesis and the below syntax was supported,
-- - @One 1@
-- - @Many ([1])@ -- only difference here is the boundary tokens surrounding the value
-- - @Many ([1, "a", 2])@
-- - @Many ([1, "a", 2, "b", 3, "c", 4])@
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

data Instance a = Instance
  { instHead :: InstanceHead a
  , instBody :: Maybe (SourceToken, NonEmpty (InstanceBinding a))
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data InstanceBinding a
  = InstanceBindingSignature a (Labeled (Name Ident) (Type a))
  | InstanceBindingName a (ValueBindingFields a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ImportDecl a = ImportDecl
  { impAnn :: a
  , impKeyword :: SourceToken
  , impModule :: Name N.ModuleName
  , impNames :: Maybe (Maybe SourceToken, DelimitedNonEmpty (Import a))
  , impQual :: Maybe (SourceToken, Name N.ModuleName)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Import a
  = ImportValue a (Name Ident)
  | ImportOp a (Name (N.OpName 'N.ValueOpName))
  | ImportType a (Name (N.ProperName 'N.TypeName)) (Maybe (DataMembers a))
  | ImportTypeOp a SourceToken (Name (N.OpName 'N.TypeOpName))
  | ImportClass a SourceToken (Name (N.ProperName 'N.ClassName))
  | ImportKind a SourceToken (Name (N.ProperName 'N.TypeName))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DataHead a = DataHead
  { dataHdKeyword :: SourceToken
  , dataHdName :: Name (N.ProperName 'N.TypeName)
  , dataHdVars :: [TypeVarBinding a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DataCtor a = DataCtor
  { dataCtorAnn :: a
  , dataCtorName :: Name (N.ProperName 'N.ConstructorName)
  , dataCtorFields :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ClassHead a = ClassHead
  { clsKeyword :: SourceToken
  , clsSuper :: Maybe (OneOrDelimited (Constraint a), SourceToken)
  , clsName :: Name (N.ProperName 'N.ClassName)
  , clsVars :: [TypeVarBinding a]
  , clsFundeps :: Maybe (SourceToken, Separated ClassFundep)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ClassFundep
  = FundepDetermined SourceToken (NonEmpty (Name Ident))
  | FundepDetermines (NonEmpty (Name Ident)) SourceToken (NonEmpty (Name Ident))
  deriving (Show, Eq, Ord, Generic)

data InstanceHead a = InstanceHead
  { instKeyword :: SourceToken
  , instNameSep :: Maybe (Name Ident, SourceToken)
  , instConstraints :: Maybe (OneOrDelimited (Constraint a), SourceToken)
  , instClass :: QualifiedName (N.ProperName 'N.ClassName)
  , instTypes :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Fixity
  = Infix
  | Infixl
  | Infixr
  deriving (Show, Eq, Ord, Generic)

data FixityOp
  = FixityValue (QualifiedName (Either Ident (N.ProperName 'N.ConstructorName))) SourceToken (Name (N.OpName 'N.ValueOpName))
  | FixityType SourceToken (QualifiedName (N.ProperName 'N.TypeName)) SourceToken (Name (N.OpName 'N.TypeOpName))
  deriving (Show, Eq, Ord, Generic)

data FixityFields = FixityFields
  { fxtKeyword :: (SourceToken, Fixity)
  , fxtPrec :: (SourceToken, Integer)
  , fxtOp :: FixityOp
  } deriving (Show, Eq, Ord, Generic)

data ValueBindingFields a = ValueBindingFields
  { valName :: Name Ident
  , valBinders :: [Binder a]
  , valGuarded :: Guarded a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Guarded a
  = Unconditional SourceToken (Where a)
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

data Foreign a
  = ForeignValue (Labeled (Name Ident) (Type a))
  | ForeignData SourceToken (Labeled (Name (N.ProperName 'N.TypeName)) (Type a))
  | ForeignKind SourceToken (Name (N.ProperName 'N.TypeName))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Role = Role
  { roleTok :: SourceToken
  , roleValue :: R.Role
  } deriving (Show, Eq, Ord, Generic)

data Expr a
  = ExprHole a (Name Ident)
  | ExprSection a SourceToken
  | ExprIdent a (QualifiedName Ident)
  | ExprConstructor a (QualifiedName (N.ProperName 'N.ConstructorName))
  | ExprBoolean a SourceToken Bool
  | ExprChar a SourceToken Char
  | ExprString a SourceToken PSString
  | ExprNumber a SourceToken (Either Integer Double)
  | ExprArray a (Delimited (Expr a))
  | ExprRecord a (Delimited (RecordLabeled (Expr a)))
  | ExprParens a (Wrapped (Expr a))
  | ExprTyped a (Expr a) SourceToken (Type a)
  | ExprInfix a (Expr a) (Wrapped (Expr a)) (Expr a)
  | ExprOp a (Expr a) (QualifiedName (N.OpName 'N.ValueOpName)) (Expr a)
  | ExprOpName a (QualifiedName (N.OpName 'N.ValueOpName))
  | ExprNegate a SourceToken (Expr a)
  | ExprRecordAccessor a (RecordAccessor a)
  | ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  | ExprApp a (Expr a) (Expr a)
  | ExprLambda a (Lambda a)
  | ExprIf a (IfThenElse a)
  | ExprCase a (CaseOf a)
  | ExprLet a (LetIn a)
  | ExprDo a (DoBlock a)
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

data Where a = Where
  { whereExpr :: Expr a
  , whereBindings :: Maybe (SourceToken, NonEmpty (LetBinding a))
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data LetBinding a
  = LetBindingSignature a (Labeled (Name Ident) (Type a))
  | LetBindingName a (ValueBindingFields a)
  | LetBindingPattern a (Binder a) SourceToken (Where a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DoBlock a = DoBlock
  { doKeyword :: SourceToken
  , doStatements :: NonEmpty (DoStatement a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DoStatement a
  = DoLet SourceToken (NonEmpty (LetBinding a))
  | DoDiscard (Expr a)
  | DoBind (Binder a) SourceToken (Expr a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data AdoBlock a = AdoBlock
  { adoKeyword :: SourceToken
  , adoStatements :: [DoStatement a]
  , adoIn :: SourceToken
  , adoResult :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Binder a
  = BinderWildcard a SourceToken
  | BinderVar a (Name Ident)
  | BinderNamed a (Name Ident) SourceToken (Binder a)
  | BinderConstructor a (QualifiedName (N.ProperName 'N.ConstructorName)) [Binder a]
  | BinderBoolean a SourceToken Bool
  | BinderChar a SourceToken Char
  | BinderString a SourceToken PSString
  | BinderNumber a (Maybe SourceToken) SourceToken (Either Integer Double)
  | BinderArray a (Delimited (Binder a))
  | BinderRecord a (Delimited (RecordLabeled (Binder a)))
  | BinderParens a (Wrapped (Binder a))
  | BinderTyped a (Binder a) SourceToken (Type a)
  | BinderOp a (Binder a) (QualifiedName (N.OpName 'N.ValueOpName)) (Binder a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
