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

{-# LANGUAGE DeriveDataTypeable #-}

module PureScript.Values (
  UnaryOperator (..),
  BinaryOperator (..),
  Value (..),
  Statement (..),
  Binder (..),
  AssignmentTarget (..)
) where

import PureScript.Types

import Data.Data

data UnaryOperator
  = Negate
  | Not
  | BitwiseNot deriving (Show, Eq, Data, Typeable)

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
  | Concat deriving (Show, Eq, Data, Typeable)

data Value
  = NumericLiteral (Either Integer Double)
  | StringLiteral String
  | BooleanLiteral Bool
  | Unary UnaryOperator Value
  | Binary BinaryOperator Value Value
  | ArrayLiteral [Value]
  | Indexer Value Value
  | ObjectLiteral [(String, Value)]
  | Accessor String Value
  | Abs [String] Value
  | App Value [Value]
  | Var String
  | Block [Statement]
  | Constructor String
  | Case Value [(Binder, Value)]
  | TypedValue Value Type deriving (Show, Data, Typeable)

data Statement
  = VariableIntroduction String Value
  | Assignment AssignmentTarget Value
  | While Value [Statement]
  | For (Statement, Value, Statement) [Statement]
  | IfThenElse (Value) [Statement] (Maybe [Statement])
  | Return Value deriving (Show, Data, Typeable)

data Binder
  = BooleanBinder Bool
  | StringBinder String
  | NumberBinder (Either Integer Double)
  | VarBinder String
  | NullaryBinder String
  | UnaryBinder String Binder
  | ObjectBinder [(String, Binder)]
  | ArrayBinder [Binder] (Maybe Binder) deriving (Show, Data, Typeable)

data AssignmentTarget
  = AssignVariable String
  | AssignObjectProperty String AssignmentTarget
  | AssignArrayIndex Value AssignmentTarget deriving (Show, Data, Typeable)
