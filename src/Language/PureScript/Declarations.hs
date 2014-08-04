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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Language.PureScript.Declarations where

import Data.Monoid (Monoid(..), mconcat)

import qualified Data.Data as D

import Control.Applicative
import Control.Monad

import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Environment
import Language.PureScript.Traversals

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
  = DataDeclaration DataDeclType ProperName [String] [(ProperName, [Type])]
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
  -- A module import (module name, optional set of identifiers to import,
  -- optional set of identifiers to hide, optional "qualified as" name)
  --
  | ImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName)
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
  -- An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  --
  | TypeClassDictionaryConstructorApp (Qualified ProperName) Value
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary Bool (Qualified ProperName, [Type]) [TypeClassDictionaryInScope]
  -- |
  -- A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  --
  | SuperClassDictionary (Qualified ProperName) [Type]
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
binderNames :: Binder -> [Ident]
binderNames = go []
  where
  go ns (VarBinder name) = name : ns
  go ns (ConstructorBinder _ bs) = foldl go ns bs
  go ns (ObjectBinder bs) = foldl go ns (map snd bs)
  go ns (ArrayBinder bs) = foldl go ns bs
  go ns (ConsBinder b1 b2) = go (go ns b1) b2
  go ns (NamedBinder name b) = go (name : ns) b
  go ns (PositionedBinder _ b) = go ns b
  go ns _ = ns

--
-- Traversals
--

everywhereOnValues :: (Declaration -> Declaration) ->
                      (Value -> Value) ->
                      (Binder -> Binder) ->
                      (Declaration -> Declaration, Value -> Value, Binder -> Binder)
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration name nameKind bs grd val) = f (ValueDeclaration name nameKind (map h' bs) (fmap g' grd) (g' val))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration name args implies ds) = f (TypeClassDeclaration name args implies (map f' ds))
  f' (TypeInstanceDeclaration name cs className args ds) = f (TypeInstanceDeclaration name cs className args (map f' ds))
  f' (PositionedDeclaration pos d) = f (PositionedDeclaration pos (f' d))
  f' other = f other

  g' :: Value -> Value
  g' (UnaryMinus v) = g (UnaryMinus (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens op (g' v1) (g' v2))
  g' (Parens v) = g (Parens (g' v))
  g' (ArrayLiteral vs) = g (ArrayLiteral (map g' vs))
  g' (ObjectLiteral vs) = g (ObjectLiteral (map (fmap g') vs))
  g' (TypeClassDictionaryConstructorApp name v) = g (TypeClassDictionaryConstructorApp name (g' v))
  g' (Accessor prop v) = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (fmap g') vs))
  g' (Abs name v) = g (Abs name (g' v))
  g' (App v1 v2) = g (App (g' v1) (g' v2))
  g' (IfThenElse v1 v2 v3) = g (IfThenElse (g' v1) (g' v2) (g' v3))
  g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue check v ty) = g (TypedValue check (g' v) ty)
  g' (Let ds v) = g (Let (map f' ds) (g' v))
  g' (Do es) = g (Do (map handleDoNotationElement es))
  g' (PositionedValue pos v) = g (PositionedValue pos (g' v))
  g' other = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ctor bs) = h (ConstructorBinder ctor (map h' bs))
  h' (ObjectBinder bs) = h (ObjectBinder (map (fmap h') bs))
  h' (ArrayBinder bs) = h (ArrayBinder (map h' bs))
  h' (ConsBinder b1 b2) = h (ConsBinder (h' b1) (h' b2))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' (PositionedBinder pos b) = h (PositionedBinder pos (h' b))
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeGuard = fmap g' (caseAlternativeGuard ca)
       , caseAlternativeResult = g' (caseAlternativeResult ca)
       }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos (handleDoNotationElement e)


everywhereOnValuesTopDownM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Value -> m Value) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Value -> m Value, Binder -> m Binder)
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> mapM (f' <=< f) ds
  f' (ValueDeclaration name nameKind bs grd val) = ValueDeclaration name nameKind <$> mapM (h' <=< h) bs <*> maybeM (g' <=< g) grd <*> (g val >>= g')
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> mapM (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> mapM (f' <=< f) ds
  f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> mapM (f' <=< f) ds
  f' (PositionedDeclaration pos d) = PositionedDeclaration pos <$> (f d >>= f')
  f' other = f other

  g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) = BinaryNoParens op <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v) = Parens <$> (g v >>= g')
  g' (ArrayLiteral vs) = ArrayLiteral <$> mapM (g' <=< g) vs
  g' (ObjectLiteral vs) = ObjectLiteral <$> mapM (sndM (g' <=< g)) vs
  g' (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g v >>= g')
  g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> mapM (sndM (g' <=< g)) vs
  g' (Abs name v) = Abs name <$> (g v >>= g')
  g' (App v1 v2) = App <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts) = Case <$> mapM (g' <=< g) vs <*> mapM handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let ds v) = Let <$> mapM (f' <=< f) ds <*> (g v >>= g')
  g' (Do es) = Do <$> mapM handleDoNotationElement es
  g' (PositionedValue pos v) = PositionedValue pos <$> (g v >>= g')
  g' other = g other

  h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> mapM (h' <=< h) bs
  h' (ObjectBinder bs) = ObjectBinder <$> mapM (sndM (h' <=< h)) bs
  h' (ArrayBinder bs) = ArrayBinder <$> mapM (h' <=< h) bs
  h' (ConsBinder b1 b2) = ConsBinder <$> (h b1 >>= h') <*> (h b2 >>= h')
  h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
  h' (PositionedBinder pos b) = PositionedBinder pos <$> (h b >>= h')
  h' other = h other

  handleCaseAlternative (CaseAlternative bs grd val) = CaseAlternative <$> mapM (h' <=< h) bs
                                                                       <*> maybeM (g' <=< g) grd
                                                                       <*> (g' <=< g) val

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> mapM (f' <=< f) ds
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos <$> handleDoNotationElement e

everywhereOnValuesM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Value -> m Value) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Value -> m Value, Binder -> m Binder)
everywhereOnValuesM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> mapM f' ds) >>= f
  f' (ValueDeclaration name nameKind bs grd val) = (ValueDeclaration name nameKind <$> mapM h' bs <*> maybeM g' grd <*> g' val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> mapM (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration name args implies ds) = (TypeClassDeclaration name args implies <$> mapM f' ds) >>= f
  f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> mapM f' ds) >>= f
  f' (PositionedDeclaration pos d) = (PositionedDeclaration pos <$> f' d) >>= f
  f' other = f other

  g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) = (BinaryNoParens op <$> (g' v1) <*> (g' v2)) >>= g
  g' (Parens v) = (Parens <$> g' v) >>= g
  g' (ArrayLiteral vs) = (ArrayLiteral <$> mapM g' vs) >>= g
  g' (ObjectLiteral vs) = (ObjectLiteral <$> mapM (sndM g') vs) >>= g
  g' (TypeClassDictionaryConstructorApp name v) = (TypeClassDictionaryConstructorApp name <$> g' v) >>= g
  g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> mapM (sndM g') vs) >>= g
  g' (Abs name v) = (Abs name <$> g' v) >>= g
  g' (App v1 v2) = (App <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts) = (Case <$> mapM g' vs <*> mapM handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let ds v) = (Let <$> mapM f' ds <*> g' v) >>= g
  g' (Do es) = (Do <$> mapM handleDoNotationElement es) >>= g
  g' (PositionedValue pos v) = (PositionedValue pos <$> g' v) >>= g
  g' other = g other

  h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> mapM h' bs) >>= h
  h' (ObjectBinder bs) = (ObjectBinder <$> mapM (sndM h') bs) >>= h
  h' (ArrayBinder bs) = (ArrayBinder <$> mapM h' bs) >>= h
  h' (ConsBinder b1 b2) = (ConsBinder <$> h' b1 <*> h' b2) >>= h
  h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
  h' (PositionedBinder pos b) = (PositionedBinder pos <$> h' b) >>= h
  h' other = h other

  handleCaseAlternative (CaseAlternative bs grd val) = CaseAlternative <$> mapM h' bs
                                                                       <*> maybeM g' grd
                                                                       <*> g' val

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> mapM f' ds
  handleDoNotationElement (PositionedDoNotationElement pos e) = PositionedDoNotationElement pos <$> handleDoNotationElement e

everythingOnValues :: (r -> r -> r) ->
                      (Declaration -> r) ->
                      (Value -> r) ->
                      (Binder -> r) ->
                      (CaseAlternative -> r) ->
                      (DoNotationElement -> r) ->
                      (Declaration -> r, Value -> r, Binder -> r, CaseAlternative -> r, DoNotationElement -> r)
everythingOnValues (<>) f g h i j = (f', g', h', i', j')
  where
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs Nothing val) = foldl (<>) (f d) (map h' bs) <> g' val
  f' d@(ValueDeclaration _ _ bs (Just grd) val) = foldl (<>) (f d) (map h' bs) <> g' grd <> g' val
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ d1) = f d <> f' d1
  f' d = f d

  g' v@(UnaryMinus v1) = g v <> g' v1
  g' v@(BinaryNoParens _ v1 v2) = g v <> g' v1 <> g' v2
  g' v@(Parens v1) = g v <> g' v1
  g' v@(ArrayLiteral vs) = foldl (<>) (g v) (map g' vs)
  g' v@(ObjectLiteral vs) = foldl (<>) (g v) (map (g' . snd) vs)
  g' v@(TypeClassDictionaryConstructorApp _ v1) = g v <> g' v1
  g' v@(Accessor _ v1) = g v <> g' v1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(Abs _ v1) = g v <> g' v1
  g' v@(App v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _) = g v <> g' v1
  g' v@(Let ds v1) = (foldl (<>) (g v) (map f' ds)) <> g' v1
  g' v@(Do es) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ v1) = g v <> g' v1
  g' v = g v

  h' b@(ConstructorBinder _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ObjectBinder bs) = foldl (<>) (h b) (map (h' . snd) bs)
  h' b@(ArrayBinder bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ConsBinder b1 b2) = h b <> h' b1 <> h' b2
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ b1) = h b <> h' b1
  h' b = h b

  i' ca = case caseAlternativeGuard ca of
    Nothing -> foldl (<>) (i ca) (map h' (caseAlternativeBinders ca)) <> g' (caseAlternativeResult ca)
    Just grd -> foldl (<>) (i ca) (map h' (caseAlternativeBinders ca)) <> g' grd <> g' (caseAlternativeResult ca)

  j' e@(DoNotationValue v) = j e <> g' v
  j' e@(DoNotationBind b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet ds) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ e1) = j e <> j' e1

everythingWithContextOnValues ::
  s ->
  r ->
  (r -> r -> r) ->
  (s -> Declaration       -> (s, r)) ->
  (s -> Value             -> (s, r)) ->
  (s -> Binder            -> (s, r)) ->
  (s -> CaseAlternative   -> (s, r)) ->
  (s -> DoNotationElement -> (s, r)) ->
  ( Declaration       -> r
  , Value             -> r
  , Binder            -> r
  , CaseAlternative   -> r
  , DoNotationElement -> r)
everythingWithContextOnValues s0 r0 (<>) f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs Nothing val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) val
  f' s (ValueDeclaration _ _ bs (Just grd) val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) grd <> (g'' s) val
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (map (\(_, _, val) -> (g'' s) val) ds)
  f' s (TypeClassDeclaration _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ d1) = (f'' s) d1
  f' _ _ = r0

  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' s (UnaryMinus v1) = (g'' s) v1
  g' s (BinaryNoParens _ v1 v2) = (g'' s) v1 <> (g'' s) v2
  g' s (Parens v1) = (g'' s) v1
  g' s (ArrayLiteral vs) = foldl (<>) r0 (map (g'' s) vs)
  g' s (ObjectLiteral vs) = foldl (<>) r0 (map (g'' s . snd) vs)
  g' s (TypeClassDictionaryConstructorApp _ v1) = (g'' s) v1
  g' s (Accessor _ v1) = (g'' s) v1
  g' s (ObjectUpdate obj vs) = foldl (<>) ((g'' s) obj) (map (g'' s . snd) vs)
  g' s (Abs _ v1) = (g'' s) v1
  g' s (App v1 v2) = (g'' s) v1 <> (g'' s) v2
  g' s (IfThenElse v1 v2 v3) = (g'' s) v1 <> (g'' s) v2 <> (g'' s) v3
  g' s (Case vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _) = (g'' s) v1
  g' s (Let ds v1) = (foldl (<>) r0 (map (f'' s) ds)) <> (g'' s) v1
  g' s (Do es) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ v1) = (g'' s) v1
  g' _ _ = r0

  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' s (ConstructorBinder _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ObjectBinder bs) = foldl (<>) r0 (map (h'' s . snd) bs)
  h' s (ArrayBinder bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ConsBinder b1 b2) = (h'' s) b1 <> (h'' s) b2
  h' s (NamedBinder _ b1) = (h'' s) b1
  h' s (PositionedBinder _ b1) = (h'' s) b1
  h' _ _ = r0

  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' s (CaseAlternative bs Nothing val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) val
  i' s (CaseAlternative bs (Just grd) val) = foldl (<>) r0 (map (h'' s) bs) <> (g'' s) grd <> (g'' s) val

  j'' s e = let (s', r) = j s e in r <> j' s' e

  j' s (DoNotationValue v) = (g'' s) v
  j' s (DoNotationBind b v) =  (h'' s) b <> (g'' s) v
  j' s (DoNotationLet ds) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ e1) = (j'' s) e1

everywhereWithContextOnValuesM :: (Functor m, Applicative m, Monad m) =>
  s ->
  (s -> Declaration       -> m (s, Declaration)) ->
  (s -> Value             -> m (s, Value)) ->
  (s -> Binder            -> m (s, Binder)) ->
  (s -> CaseAlternative   -> m (s, CaseAlternative)) ->
  (s -> DoNotationElement -> m (s, DoNotationElement)) ->
  ( Declaration       -> m Declaration
  , Value             -> m Value
  , Binder            -> m Binder
  , CaseAlternative   -> m CaseAlternative
  , DoNotationElement -> m DoNotationElement)
everywhereWithContextOnValuesM s0 f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> mapM (f'' s) ds
  f' s (ValueDeclaration name nameKind bs grd val) = ValueDeclaration name nameKind <$> mapM (h'' s) bs <*> maybeM (g'' s) grd <*> g'' s val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> mapM (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> mapM (f'' s) ds
  f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> mapM (f'' s) ds
  f' s (PositionedDeclaration pos d1) = PositionedDeclaration pos <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
  g' s (BinaryNoParens op v1 v2) = BinaryNoParens op <$> g'' s v1 <*> g'' s v2
  g' s (Parens v) = Parens <$> g'' s v
  g' s (ArrayLiteral vs) = ArrayLiteral <$> mapM (g'' s) vs
  g' s (ObjectLiteral vs) = ObjectLiteral <$> mapM (sndM (g'' s)) vs
  g' s (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g'' s) v
  g' s (Accessor prop v) = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> mapM (sndM (g'' s)) vs
  g' s (Abs name v) = Abs name <$> g'' s v
  g' s (App v1 v2) = App <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts) = Case <$> mapM (g'' s) vs <*> mapM (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let ds v) = Let <$> mapM (f'' s) ds <*> g'' s v
  g' s (Do es) = Do <$> mapM (j'' s) es
  g' s (PositionedValue pos v) = PositionedValue pos <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> mapM (h'' s) bs
  h' s (ObjectBinder bs) = ObjectBinder <$> mapM (sndM (h'' s)) bs
  h' s (ArrayBinder bs) = ArrayBinder <$> mapM (h'' s) bs
  h' s (ConsBinder b1 b2) = ConsBinder <$> h'' s b1 <*> h'' s b2
  h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
  h' s (PositionedBinder pos b) = PositionedBinder pos <$> h'' s b
  h' _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs grd val) = CaseAlternative <$> mapM (h'' s) bs <*> maybeM (g'' s) grd <*> g'' s val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue v) = DoNotationValue <$> g'' s v
  j' s (DoNotationBind b v) = DoNotationBind <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ds) = DoNotationLet <$> mapM (f'' s) ds
  j' s (PositionedDoNotationElement pos e1) = PositionedDoNotationElement pos <$> j'' s e1

accumTypes :: (Monoid r) => (Type -> r) -> (Declaration -> r, Value -> r, Binder -> r, CaseAlternative -> r, DoNotationElement -> r)
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ dctors) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ _ _ ty) = f ty
  forDecls (ExternInstanceDeclaration _ cs _ tys) = mconcat (concatMap (map f . snd) cs) `mappend` mconcat (map f tys)
  forDecls (TypeClassDeclaration _ _ implies _) = mconcat (concatMap (map f . snd) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f . snd) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty) = f ty
  forDecls (TypeDeclaration _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary _ (_, cs) _) = mconcat (map f cs)
  forValues (SuperClassDictionary _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ ty) = f ty
  forValues _ = mempty


