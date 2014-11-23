-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Expr
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

module Language.PureScript.CoreFn.Expr where

import qualified Data.Data as D

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Meta
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- Data type for expressions and terms
--
data Expr
  -- |
  -- A literal value
  --
  = Literal (Literal Expr)
  -- |
  -- A record property accessor
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
  | Let [Bind] Expr
  -- |
  -- Metadata annotations for preserving information about an expression that
  -- may be useful for certain optimizations or codegen targets.
  --
  | Meta Meta Expr deriving (Show, D.Data, D.Typeable)

-- |
-- A let or module binding.
--
data Bind
  -- |
  -- Non-recursive binding for a single value
  --
  = NotRec Ident Expr
  -- |
  -- Mutually recursive binding group for several values
  --
  | Rec [(Ident, Expr)] deriving (Show, D.Data, D.Typeable)

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
