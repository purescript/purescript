{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data types and functions for representing a simplified form of PureScript
-- code, intended for use in e.g. HTML documentation.

module Language.PureScript.Docs.RenderedCode.Types
 ( RenderedCodeElement(..)
 , asRenderedCodeElement
 , ContainingModule(..)
 , asContainingModule
 , containingModuleToMaybe
 , maybeToContainingModule
 , fromContainingModule
 , RenderedCode
 , asRenderedCode
 , outputWith
 , sp
 , syntax
 , ident
 , ident'
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
 , keywordFixity
 , keywordKind
 ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))

import Data.Aeson.BetterErrors
import qualified Data.Aeson as A
import Data.Text (Text)

import qualified Language.PureScript as P

-- |
-- A single element in a rendered code fragment. The intention is to support
-- multiple output formats. For example, plain text, or highlighted HTML.
--
data RenderedCodeElement
  = Syntax Text
  | Ident Text ContainingModule
  | Ctor Text ContainingModule
  | Kind Text
  | Keyword Text
  | Space
  deriving (Show, Eq, Ord)

instance A.ToJSON RenderedCodeElement where
  toJSON (Syntax str) =
    A.toJSON ["syntax", str]
  toJSON (Ident str mn) =
    A.toJSON ["ident", A.toJSON str, A.toJSON mn]
  toJSON (Ctor str mn) =
    A.toJSON ["ctor", A.toJSON str, A.toJSON mn ]
  toJSON (Kind str) =
    A.toJSON ["kind", str]
  toJSON (Keyword str) =
    A.toJSON ["keyword", str]
  toJSON Space =
    A.toJSON ["space" :: Text]

asRenderedCodeElement :: Parse Text RenderedCodeElement
asRenderedCodeElement =
  a Syntax "syntax" <|>
  asIdent <|>
  asCtor <|>
  a Kind "kind" <|>
  a Keyword "keyword" <|>
  asSpace <|>
  unableToParse
  where
  p <|> q = catchError p (const q)

  a ctor' ctorStr = ctor' <$> (nth 0 (withText (eq ctorStr)) *> nth 1 asText)
  asIdent = nth 0 (withText (eq "ident")) *> (Ident <$> nth 1 asText <*> nth 2 asContainingModule)
  asCtor = nth 0 (withText (eq "ctor")) *> (Ctor <$> nth 1 asText <*> nth 2 asContainingModule)
  asSpace = nth 0 (withText (eq "space")) *> pure Space

  eq s s' = if s == s' then Right () else Left ""

  unableToParse = withText Left

-- |
-- This type is isomorphic to 'Maybe' 'P.ModuleName'. It makes code a bit easier
-- to read, as the meaning is more explicit.
--
data ContainingModule
  = ThisModule
  | OtherModule P.ModuleName
  deriving (Show, Eq, Ord)

instance A.ToJSON ContainingModule where
  toJSON mn = A.toJSON (P.runModuleName <$> containingModuleToMaybe mn)

asContainingModule :: Parse e ContainingModule
asContainingModule =
  maybeToContainingModule <$> perhaps (P.moduleNameFromString <$> asText)

-- |
-- Convert a 'Maybe' 'P.ModuleName' to a 'ContainingModule', using the obvious
-- isomorphism.
--
maybeToContainingModule :: Maybe P.ModuleName -> ContainingModule
maybeToContainingModule Nothing = ThisModule
maybeToContainingModule (Just mn) = OtherModule mn

-- |
-- Convert a 'ContainingModule' to a 'Maybe' 'P.ModuleName', using the obvious
-- isomorphism.
--
containingModuleToMaybe :: ContainingModule -> Maybe P.ModuleName
containingModuleToMaybe ThisModule = Nothing
containingModuleToMaybe (OtherModule mn) = Just mn

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

instance A.ToJSON RenderedCode where
  toJSON (RC elems) = A.toJSON elems

asRenderedCode :: Parse Text RenderedCode
asRenderedCode = RC <$> eachInArray asRenderedCodeElement

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

syntax :: Text -> RenderedCode
syntax x = RC [Syntax x]

ident :: Text -> RenderedCode
ident x = RC [Ident x ThisModule]

ident' :: Text -> ContainingModule -> RenderedCode
ident' x m = RC [Ident x m]

ctor :: Text -> ContainingModule -> RenderedCode
ctor x m = RC [Ctor x m]

kind :: Text -> RenderedCode
kind x = RC [Kind x]

keyword :: Text -> RenderedCode
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

keywordFixity :: P.Associativity -> RenderedCode
keywordFixity P.Infixl = keyword "infixl"
keywordFixity P.Infixr = keyword "infixr"
keywordFixity P.Infix = keyword "infix"

keywordKind :: RenderedCode
keywordKind = keyword "kind"
