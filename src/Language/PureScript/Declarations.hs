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
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.PureScript.Declarations where

import Language.PureScript.Values
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds

import qualified Data.Data as D

type Precedence = Integer

data Associativity = Infixl | Infixr deriving (Show, D.Data, D.Typeable)

data Fixity = Fixity Associativity Precedence deriving (Show, D.Data, D.Typeable)

data Declaration
  = DataDeclaration ProperName [String] [(ProperName, Maybe Type)]
  | TypeSynonymDeclaration ProperName [String] Type
  | TypeDeclaration Ident PolyType
  | ValueDeclaration Ident Value
  | ExternDeclaration Ident PolyType
  | ExternMemberDeclaration String Ident PolyType
  | ExternDataDeclaration ProperName Kind
  | FixityDeclaration Fixity String
  | ModuleDeclaration ProperName [Declaration]
  | ImportDeclaration [ProperName] (Maybe [Ident])
  deriving (Show, D.Data, D.Typeable)
