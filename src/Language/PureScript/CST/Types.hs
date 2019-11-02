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
import Language.PureScript.PSString (PSString)

data SourcePos = SourcePos
  { srcLine :: {-# UNPACK #-} !Int
  , srcColumn :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord, Generic)

data SourceRange = SourceRange
  { srcStart :: !SourcePos
  , srcEnd :: !SourcePos
  } deriving (Show, Eq, Ord, Generic)

data Comment l
  = Comment !Text
  | Space {-# UNPACK #-} !Int
  | Line !l
  deriving (Show, Eq, Ord, Generic, Functor)

data LineFeed = LF | CRLF
  deriving (Show, Eq, Ord, Generic)

data TokenAnn = TokenAnn
  { tokRange :: !SourceRange
  , tokLeadingComments :: ![Comment LineFeed]
  , tokTrailingComments :: ![Comment Void]
  } deriving (Show, Eq, Ord, Generic)

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

data SourceToken = SourceToken
  { tokAnn :: !TokenAnn
  , tokValue :: !Token
  } deriving (Show, Eq, Ord, Generic)

data Ident = Ident
  { getIdent :: Text
  } deriving (Show, Eq, Ord, Generic)

data Name a = Name
  { nameTok :: SourceToken
  , nameValue :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data QualifiedName a = QualifiedName
  { qualTok :: SourceToken
  , qualModule :: Maybe N.ModuleName
  , qualName :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Label = Label
  { lblTok :: SourceToken
  , lblName :: PSString
  } deriving (Show, Eq, Ord, Generic)

data Wrapped a = Wrapped
  { wrpOpen :: SourceToken
  , wrpValue :: a
  , wrpClose :: SourceToken
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Separated a = Separated
  { sepHead :: a
  , sepTail :: [(SourceToken, a)]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Labeled a b = Labeled
  { lblLabel :: a
  , lblSep :: SourceToken
  , lblValue  :: b
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Kind a
  = KindName a (QualifiedName (N.ProperName 'N.KindName))
  | KindArr a (Kind a) SourceToken (Kind a)
  | KindRow a SourceToken (Kind a)
  | KindParens a (Wrapped (Kind a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Type a
  = TypeVar a (Name Ident)
  | TypeConstructor a (QualifiedName (N.ProperName 'N.TypeName))
  | TypeWildcard a SourceToken
  | TypeHole a (Name Ident)
  | TypeString a SourceToken PSString
  | TypeRow a (Wrapped (Row a))
  | TypeRecord a (Wrapped (Row a))
  | TypeForall a SourceToken (NonEmpty (TypeVarBinding a)) SourceToken (Type a)
  | TypeKinded a (Type a) SourceToken (Kind a)
  | TypeApp a (Type a) (Type a)
  | TypeOp a (Type a) (QualifiedName (N.OpName 'N.TypeOpName)) (Type a)
  | TypeOpName a (QualifiedName (N.OpName 'N.TypeOpName))
  | TypeArr a (Type a) SourceToken (Type a)
  | TypeArrName a SourceToken
  | TypeConstrained a (Constraint a) SourceToken (Type a)
  | TypeParens a (Wrapped (Type a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data TypeVarBinding a
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (Kind a)))
  | TypeVarName (Name Ident)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Constraint a
  = Constraint a (QualifiedName (N.ProperName 'N.ClassName)) [Type a]
  | ConstraintParens a (Wrapped (Constraint a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Row a = Row
  { rowLabels :: Maybe (Separated (Labeled Label (Type a)))
  , rowTail :: Maybe (SourceToken, Type a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

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

data Export a
  = ExportValue a (Name Ident)
  | ExportOp a (Name (N.OpName 'N.ValueOpName))
  | ExportType a (Name (N.ProperName 'N.TypeName)) (Maybe (DataMembers a))
  | ExportTypeOp a SourceToken (Name (N.OpName 'N.TypeOpName))
  | ExportClass a SourceToken (Name (N.ProperName 'N.ClassName))
  | ExportKind a SourceToken (Name (N.ProperName 'N.KindName))
  | ExportModule a SourceToken (Name N.ModuleName)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DataMembers a
  = DataAll a SourceToken
  | DataEnumerated a (Delimited (Name (N.ProperName 'N.ConstructorName)))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Declaration a
  = DeclData a (DataHead a) (Maybe (SourceToken, Separated (DataCtor a)))
  | DeclType a (DataHead a) SourceToken (Type a)
  | DeclNewtype a (DataHead a) SourceToken (Name (N.ProperName 'N.ConstructorName)) (Type a)
  | DeclClass a (ClassHead a) (Maybe (SourceToken, NonEmpty (Labeled (Name Ident) (Type a))))
  | DeclInstanceChain a (Separated (Instance a))
  | DeclDerive a SourceToken (Maybe SourceToken) (InstanceHead a)
  | DeclSignature a (Labeled (Name Ident) (Type a))
  | DeclValue a (ValueBindingFields a)
  | DeclFixity a FixityFields
  | DeclForeign a SourceToken SourceToken (Foreign a)
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
  | ImportKind a SourceToken (Name (N.ProperName 'N.KindName))
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
  , instName :: Name Ident
  , instSep :: SourceToken
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
  | ForeignData SourceToken (Labeled (Name (N.ProperName 'N.TypeName)) (Kind a))
  | ForeignKind SourceToken (Name (N.ProperName 'N.KindName))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

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
