-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Values
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for values, statements, binders and do notation
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Values where

import Language.PureScript.Types
import Language.PureScript.Names

import Data.Data

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard = Value

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive deriving (Show, Eq, Data, Typeable)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- |
  -- Bitwise right shift with zero-fill
  --
  | ZeroFillShiftRight
  -- |
  -- String concatenation
  --
  | Concat deriving (Show, Eq, Data, Typeable)

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
  -- Unary operator application
  --
  | Unary UnaryOperator Value
  -- |
  -- Binary operator application
  --
  | Binary BinaryOperator Value Value
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
  -- An array indexing expression
  --
  | Indexer Value Value
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
  | Abs Ident Value
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
  -- A \"Block\" i.e. a collection of statements which evaluate to a value
  --
  | Block [Statement]
  -- |
  -- A data constructor
  --
  | Constructor (Qualified ProperName)
  -- |
  -- A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  --
  | Case [Value] [([Binder], Maybe Guard, Value)]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Bool Value Type
  -- |
  -- A do-notation block
  --
  | Do [DoNotationElement]
  -- |
  -- A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): the type class name and
  -- instance type, and the type class dictionaries in scope.
  --
  | TypeClassDictionary (Qualified ProperName, Type) [TypeClassDictionaryInScope] deriving (Show, Data, Typeable)

-- |
-- The type of a type class dictionary
--
data TypeClassDictionaryType
  -- |
  -- A regular type class dictionary
  --
  = TCDRegular
  -- |
  -- A type class dictionary which is an alias for an imported dictionary from another module
  --
  | TCDAlias (Qualified Ident) deriving (Show, Eq, Data, Typeable)

-- |
-- Data representing a type class dictionary which is in scope
--
data TypeClassDictionaryInScope
  = TypeClassDictionaryInScope {
    -- |
    -- The identifier with which the dictionary can be accessed at runtime
    --
      tcdName :: Qualified Ident
    -- |
    -- The name of the type class to which this type class instance applies
    --
    , tcdClassName :: Qualified ProperName
    -- |
    -- The type to which this type class instance applies
    --
    , tcdInstanceType :: Type
    -- |
    -- Type class dependencies which must be satisfied to construct this dictionary
    --
    , tcdDependencies :: Maybe [(Qualified ProperName, Type)]
    -- |
    -- The type of this dictionary
    --
    , tcdType :: TypeClassDictionaryType
    } deriving (Show, Data, Typeable)

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
  | DoNotationLet Binder Value deriving (Show, Data, Typeable)

-- |
-- Data type for statements which can appear inside a @Block@ expression
--
data Statement
  -- |
  -- A variable introduction and initial assignment
  --
  = VariableIntroduction Ident Value
  -- |
  -- A variable reassignment
  --
  | Assignment Ident Value
  -- |
  -- A while loop
  --
  | While Value [Statement]
  -- |
  -- A for loop
  --
  | For Ident Value Value [Statement]
  -- |
  -- An if-then-else statement
  --
  | If IfStatement
  -- |
  -- A return statement
  --
  | Return Value deriving (Show, Data, Typeable)

-- |
-- Data type for if-statements
--
data IfStatement
  -- |
  -- An if statement. Arguments are (in order): boolean condition, true branch, optional else branch.
  --
  = IfStatement Value [Statement] (Maybe ElseStatement) deriving (Show, Data, Typeable)

-- |
-- Data type for the else branch in an if-statement
--
data ElseStatement
  -- |
  -- An else branch
  --
  = Else [Statement]
  -- |
  -- An else-if branch
  --
  | ElseIf IfStatement deriving (Show, Data, Typeable)

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
  -- A binder which matches a data constructor with no argument
  --
  | NullaryBinder (Qualified ProperName)
  -- |
  -- A binder which matches a data constructor with one argument
  --
  | UnaryBinder (Qualified ProperName) Binder
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
  | NamedBinder Ident Binder deriving (Show, Data, Typeable)
