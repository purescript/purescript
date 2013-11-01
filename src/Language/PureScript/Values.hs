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
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Values where

import Language.PureScript.Types
import Language.PureScript.Names

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
  | ObjectUpdate Value [(String, Value)]
  | Abs [Ident] Value
  | App Value [Value]
  | Var Ident
  | IfThenElse Value Value Value
  | Block [Statement]
  | Constructor String
  | Case Value [(Binder, Value)]
  | TypedValue Value PolyType deriving (Show, Data, Typeable)

data Statement
  = VariableIntroduction Ident Value
  | Assignment Ident Value
  | While Value [Statement]
  | For Ident Value Value [Statement]
  | ForEach Ident Value [Statement]
  | If IfStatement
  | Return Value deriving (Show, Data, Typeable)

data IfStatement = IfStatement Value [Statement] (Maybe ElseStatement) deriving (Show, Data, Typeable)

data ElseStatement
  = Else [Statement]
  | ElseIf IfStatement deriving (Show, Data, Typeable)

data Binder
  = NullBinder
  | BooleanBinder Bool
  | StringBinder String
  | NumberBinder (Either Integer Double)
  | VarBinder Ident
  | NullaryBinder String
  | UnaryBinder String Binder
  | ObjectBinder [(String, Binder)]
  | ArrayBinder [Binder] (Maybe Binder)
  | NamedBinder Ident Binder
  | GuardedBinder Value Binder deriving (Show, Data, Typeable)
