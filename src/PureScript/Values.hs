-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Values
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript.Values (
  UnaryOperator (..),
  BinaryOperator (..),
  Value (..),
  Statement (..),
  Binder (..)
) where

import PureScript.Types

data UnaryOperator
  = Negate
  | Not
  | BitwiseNot deriving (Show, Eq)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ZeroFillShiftRight
  | Concat deriving (Show, Eq)

data Value
  = NumericLiteral Double
  | StringLiteral String
  | BooleanLiteral Bool
  | Unary UnaryOperator Value
  | Binary BinaryOperator Value Value
  | ArrayLiteral [Value]
  | ObjectLiteral [(String, Value)]
  | Accessor String Value
  | Abs [String] Value
  | App Value [Value]
  | Var String
  | Block [Statement]
  | Constructor String
  | Case Value [(Binder, Value)]
  | TypedValue Value Type deriving Show

data Statement
  = VariableIntroduction String Value
  | Assignment String Value
  | While Value [Statement]
  | For (Statement, Value, Statement) [Statement]
  | IfThenElse (Value) [Statement] (Maybe [Statement])
  | Return Value deriving Show

data Binder
  = VarBinder String
  | ConstructorBinder String Binder
  | ObjectBinder [(String, Binder)] deriving Show
