-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The core functional representation
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.CoreFn where

import qualified Data.Data as D

import Language.PureScript.AST.SourcePos
import Language.PureScript.Types
import Language.PureScript.Names

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
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
  -- An array literal
  --
  | ArrayLiteral [a]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(String, a)] deriving (Show, D.Data, D.Typeable)

-- |
-- Data type for expressions and terms
--
data Expr
  -- |
  -- A literal value
  --
  = Literal (Literal Expr)
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
  | Abs Ident Expr
  -- |
  -- Function application
  --
  | App Expr Expr
  -- |
  -- Variable
  --
  | Var (Qualified Ident)
  -- |
  -- A data constructor
  --
  | Constructor (Qualified ProperName)
  -- |
  -- A case expression
  --
  | Case [Expr] [CaseAlternative]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Expr Type
  -- |
  -- A let binding
  --
  | Let [(Ident, Expr)] Expr
  -- |
  -- An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  --
  | TypeClassDictionaryConstructorApp (Qualified ProperName) Expr deriving (Show, D.Data, D.Typeable)

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard = Expr

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
  } deriving (Show, D.Data, D.Typeable)

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
  | LiteralBinder (Literal Binder)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified ProperName) [Binder]
  -- |
  -- A binder which matches an array and binds its head and tail
  --
  | ConsBinder Binder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident Binder deriving (Show, D.Data, D.Typeable)
