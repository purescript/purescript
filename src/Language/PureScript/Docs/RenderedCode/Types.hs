{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data types and functions for representing a simplified form of PureScript
-- code, intended for use in e.g. HTML documentation.

module Language.PureScript.Docs.RenderedCode.Types
 ( RenderedCodeElement(..)
 , ContainingModule(..)
 , toContainingModule
 , fromContainingModule
 , RenderedCode
 , outputWith
 , sp
 , syntax
 , ident
 , ctor
 , kind
 , keyword
 , keywordForall
 , keywordData
 , keywordNewtype
 , keywordType
 , keywordClass
 , keywordInstance
 , keywordWhere
 ) where

import Data.Foldable
import Data.Monoid

import qualified Language.PureScript as P

-- |
-- A single element in a rendered code fragment. The intention is to support
-- multiple output formats. For example, plain text, or highlighted HTML.
--
data RenderedCodeElement
  = Syntax String
  | Ident String
  | Ctor String ContainingModule
  | Kind String
  | Keyword String
  | Space
  deriving (Show, Eq, Ord)

-- |
-- This type is isomorphic to 'Maybe' 'P.ModuleName'. It makes code a bit easier
-- to read, as the meaning is more explicit.
--
data ContainingModule
  = ThisModule
  | OtherModule P.ModuleName
  deriving (Show, Eq, Ord)

-- |
-- Convert a 'Maybe' 'P.ModuleName' to a 'ContainingModule', using the obvious
-- isomorphism.
--
toContainingModule :: Maybe P.ModuleName -> ContainingModule
toContainingModule Nothing = ThisModule
toContainingModule (Just mn) = OtherModule mn

-- |
-- A version of 'fromMaybe' for 'ContainingModule' values.
--
fromContainingModule :: P.ModuleName -> ContainingModule -> P.ModuleName
fromContainingModule def ThisModule = def
fromContainingModule _ (OtherModule mn) = mn

-- |
-- A type representing a highly simplified version of PureScript code, intended
-- for use in output formats like plain text or HTML.
--
newtype RenderedCode
  = RC { unRC :: [RenderedCodeElement] }
  deriving (Show, Eq, Ord, Monoid)

-- |
-- This function allows conversion of a 'RenderedCode' value into a value of
-- some other type (for example, plain text, or HTML). The first argument
-- is a function specifying how each individual 'RenderedCodeElement' should be
-- rendered.
--
outputWith :: Monoid a => (RenderedCodeElement -> a) -> RenderedCode -> a
outputWith f = foldMap f . unRC

-- |
-- A 'RenderedCode' fragment representing a space.
--
sp :: RenderedCode
sp = RC [Space]

syntax :: String -> RenderedCode
syntax x = RC [Syntax x]

ident :: String -> RenderedCode
ident x = RC [Ident x]

ctor :: String -> ContainingModule -> RenderedCode
ctor x m = RC [Ctor x m]

kind :: String -> RenderedCode
kind x = RC [Kind x]

keyword :: String -> RenderedCode
keyword kw = RC [Keyword kw]

keywordForall :: RenderedCode
keywordForall = keyword "forall"

keywordData :: RenderedCode
keywordData = keyword "data"

keywordNewtype :: RenderedCode
keywordNewtype = keyword "newtype"

keywordType :: RenderedCode
keywordType = keyword "type"

keywordClass :: RenderedCode
keywordClass = keyword "class"

keywordInstance :: RenderedCode
keywordInstance = keyword "instance"

keywordWhere :: RenderedCode
keywordWhere = keyword "where"
