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
data Expr a
  -- |
  -- A literal value
  --
  = Literal (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, arity)
  --
  | Constructor ProperName ProperName Arity
  -- |
  -- A record property accessor
  --
  | Accessor String (Expr a)
  -- |
  -- Partial record update
  --
  | ObjectUpdate (Expr a) [(String, Expr a)]
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)
  -- |
  -- A case expression
  --
  | Case [Expr a] [CaseAlternative a]
  -- |
  -- A value with a type annotation
  --
  | TypedValue (Expr a) Type
  -- |
  -- A let binding
  --
  | Let [Bind a] (Expr a) deriving (Show, D.Data, D.Typeable)

-- |
-- A let or module binding.
--
data Bind a
  -- |
  -- Non-recursive binding for a single value
  --
  = NonRec Ident (Expr a)
  -- |
  -- Mutually recursive binding group for several values
  --
  | Rec [(Ident, Expr a)] deriving (Show, D.Data, D.Typeable)

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard a = Expr a

-- |
-- An alternative in a case statement
--
data CaseAlternative a = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder a]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: Either [(Guard a, Expr a)] (Expr a)
  } deriving (Show, D.Data, D.Typeable)
