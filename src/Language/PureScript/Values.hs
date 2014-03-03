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
import Data.Generics (mkQ, everything)

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
  | Case [Value] [([Binder], Maybe Guard, Value)]
  -- |
  -- A value with a type annotation
  --
  | TypedValue Bool Value Type
  -- |
  -- A let binding
  --
  | Let Binder Value Value
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
  | TypeClassDictionary (Qualified ProperName, [Type]) [TypeClassDictionaryInScope] deriving (Show, Data, Typeable)

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
    -- The types to which this type class instance applies
    --
    , tcdInstanceTypes :: [Type]
    -- |
    -- Type class dependencies which must be satisfied to construct this dictionary
    --
    , tcdDependencies :: Maybe [(Qualified ProperName, [Type])]
    -- |
    -- The type of this dictionary
    --
    , tcdType :: TypeClassDictionaryType
    } deriving (Show, Data, Typeable)

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
  | NamedBinder Ident Binder deriving (Show, Data, Typeable)


-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: (Data d) => d -> [Ident]
binderNames = everything (++) (mkQ [] go)
  where
  go (VarBinder ident) = [ident]
  go (NamedBinder ident _) = [ident]
  go _ = []
