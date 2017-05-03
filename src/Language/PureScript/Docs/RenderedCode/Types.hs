{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

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
 , fromQualified
 , Namespace(..)
 , Link(..)
 , FixityAlias
 , RenderedCode
 , asRenderedCode
 , outputWith
 , sp
 , parens
 , syntax
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
 , keywordAs
 , ident
 , dataCtor
 , typeCtor
 , typeOp
 , typeVar
 , kind
 , alias
 , aliasName
 ) where

import Prelude.Compat
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Monad.Error.Class (MonadError(..))

import Data.Monoid ((<>))
import Data.Aeson.BetterErrors (Parse, nth, withText, withValue, toAesonParser, perhaps, asText, eachInArray)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as TE

import Language.PureScript.Names
import Language.PureScript.AST (Associativity(..))
import Language.PureScript.Crash (internalError)

-- | Given a list of actions, attempt them all, returning the first success.
-- If all the actions fail, 'tryAll' returns the first argument.
tryAll :: MonadError e m => m a -> [m a] -> m a
tryAll = foldr $ \x y -> catchError x (const y)

firstEq :: Text -> Parse Text a -> Parse Text a
firstEq str p = nth 0 (withText (eq str)) *> p
  where
  eq s s' = if s == s' then Right () else Left ""

-- |
-- Try the given parsers in sequence. If all fail, fail with the given message,
-- and include the JSON in the error.
--
tryParse :: Text -> [Parse Text a] -> Parse Text a
tryParse msg =
  tryAll (withValue (Left . (fullMsg <>) . showJSON))

  where
  fullMsg = "Invalid " <> msg <> ": "

  showJSON :: A.Value -> Text
  showJSON = TE.decodeUtf8 . BS.toStrict . A.encode

-- |
-- This type is isomorphic to 'Maybe' 'ModuleName'. It makes code a bit
-- easier to read, as the meaning is more explicit.
--
data ContainingModule
  = ThisModule
  | OtherModule ModuleName
  deriving (Show, Eq, Ord)

instance A.ToJSON ContainingModule where
  toJSON = A.toJSON . go
    where
    go = \case
      ThisModule -> ["ThisModule"]
      OtherModule mn -> ["OtherModule", runModuleName mn]

instance A.FromJSON ContainingModule where
  parseJSON = toAesonParser id asContainingModule

asContainingModule :: Parse Text ContainingModule
asContainingModule =
  tryParse "containing module" $
    current ++ backwardsCompat
  where
  current =
    [ firstEq "ThisModule" (pure ThisModule)
    , firstEq "OtherModule" (OtherModule <$> nth 1 asModuleName)
    ]

  -- For JSON produced by compilers up to 0.10.5.
  backwardsCompat =
    [ maybeToContainingModule <$> perhaps asModuleName
    ]

  asModuleName = moduleNameFromString <$> asText

-- |
-- Convert a 'Maybe' 'ModuleName' to a 'ContainingModule', using the obvious
-- isomorphism.
--
maybeToContainingModule :: Maybe ModuleName -> ContainingModule
maybeToContainingModule Nothing = ThisModule
maybeToContainingModule (Just mn) = OtherModule mn

-- |
-- Convert a 'ContainingModule' to a 'Maybe' 'ModuleName', using the obvious
-- isomorphism.
--
containingModuleToMaybe :: ContainingModule -> Maybe ModuleName
containingModuleToMaybe ThisModule = Nothing
containingModuleToMaybe (OtherModule mn) = Just mn

-- |
-- A version of 'fromMaybe' for 'ContainingModule' values.
--
fromContainingModule :: ModuleName -> ContainingModule -> ModuleName
fromContainingModule def ThisModule = def
fromContainingModule _ (OtherModule mn) = mn

fromQualified :: Qualified a -> (ContainingModule, a)
fromQualified (Qualified mn x) =
  (maybeToContainingModule mn, x)

data Link
  = NoLink
  | Link ContainingModule
  deriving (Show, Eq, Ord)

instance A.ToJSON Link where
  toJSON = \case
    NoLink -> A.toJSON ["NoLink" :: Text]
    Link mn -> A.toJSON ["Link", A.toJSON mn]

asLink :: Parse Text Link
asLink =
  tryParse "link"
    [ firstEq "NoLink" (pure NoLink)
    , firstEq "Link" (Link <$> nth 1 asContainingModule)
    ]

instance A.FromJSON Link where
  parseJSON = toAesonParser id asLink

data Namespace
  = ValueLevel
  | TypeLevel
  | KindLevel
  deriving (Show, Eq, Ord, Generic)

instance NFData Namespace

instance A.ToJSON Namespace where
  toJSON = A.toJSON . show

asNamespace :: Parse Text Namespace
asNamespace =
  tryParse "namespace"
    [ withText $ \case
        "ValueLevel" -> Right ValueLevel
        "TypeLevel" -> Right TypeLevel
        "KindLevel" -> Right KindLevel
        _ -> Left ""
    ]

instance A.FromJSON Namespace where
  parseJSON = toAesonParser id asNamespace

-- |
-- A single element in a rendered code fragment. The intention is to support
-- multiple output formats. For example, plain text, or highlighted HTML.
--
data RenderedCodeElement
  = Syntax Text
  | Keyword Text
  | Space
  -- | Any symbol which you might or might not want to link to, in any
  -- namespace (value, type, or kind). Note that this is not related to the
  -- kind called Symbol for type-level strings.
  | Symbol Namespace Text Link
  deriving (Show, Eq, Ord)

instance A.ToJSON RenderedCodeElement where
  toJSON (Syntax str) =
    A.toJSON ["syntax", str]
  toJSON (Keyword str) =
    A.toJSON ["keyword", str]
  toJSON Space =
    A.toJSON ["space" :: Text]
  toJSON (Symbol ns str link) =
    A.toJSON ["symbol", A.toJSON ns, A.toJSON str, A.toJSON link]

asRenderedCodeElement :: Parse Text RenderedCodeElement
asRenderedCodeElement =
  tryParse "RenderedCodeElement" $
    [ a Syntax "syntax"
    , a Keyword "keyword"
    , asSpace
    , asSymbol
    ] ++ backwardsCompat
  where
  a ctor' ctorStr = firstEq ctorStr (ctor' <$> nth 1 asText)
  asSymbol = firstEq "symbol" (Symbol <$> nth 1 asNamespace <*> nth 2 asText <*> nth 3 asLink)
  asSpace = firstEq "space" (pure Space)

  -- These will make some mistakes e.g. treating data constructors as types,
  -- because the old code did not save information which is necessary to
  -- distinguish these cases. This is the best we can do.
  backwardsCompat =
    [ oldAsIdent
    , oldAsCtor
    , oldAsKind
    ]

  oldAsIdent = firstEq "ident" (Symbol ValueLevel <$> nth 1 asText <*> nth 2 (Link <$> asContainingModule))
  oldAsCtor = firstEq "ctor" (Symbol TypeLevel <$> nth 1 asText <*> nth 2 (Link <$> asContainingModule))
  oldAsKind = firstEq "kind" (Symbol KindLevel <$> nth 1 asText <*> pure (Link ThisModule))

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

-- |
-- Wrap a RenderedCode value in parens.
parens :: RenderedCode -> RenderedCode
parens x = syntax "(" <> x <> syntax ")"

-- possible TODO: instead of this function, export RenderedCode values for
-- each syntax element, eg syntaxArr (== syntax "->"), syntaxLBrace,
-- syntaxRBrace, etc.
syntax :: Text -> RenderedCode
syntax x = RC [Syntax x]

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

keywordFixity :: Associativity -> RenderedCode
keywordFixity Infixl = keyword "infixl"
keywordFixity Infixr = keyword "infixr"
keywordFixity Infix = keyword "infix"

keywordKind :: RenderedCode
keywordKind = keyword "kind"

keywordAs :: RenderedCode
keywordAs = keyword "as"

ident :: Qualified Ident -> RenderedCode
ident (fromQualified -> (mn, name)) =
  RC [Symbol ValueLevel (runIdent name) (Link mn)]

dataCtor :: Qualified (ProperName 'ConstructorName) -> RenderedCode
dataCtor (fromQualified -> (mn, name)) =
  RC [Symbol ValueLevel (runProperName name) (Link mn)]

typeCtor :: Qualified (ProperName 'TypeName) -> RenderedCode
typeCtor (fromQualified -> (mn, name)) =
  RC [Symbol TypeLevel (runProperName name) (Link mn)]

typeOp :: Qualified (OpName 'TypeOpName) -> RenderedCode
typeOp (fromQualified -> (mn, name)) =
  RC [Symbol TypeLevel (runOpName name) (Link mn)]

typeVar :: Text -> RenderedCode
typeVar x = RC [Symbol TypeLevel x NoLink]

kind :: Qualified (ProperName 'KindName) -> RenderedCode
kind (fromQualified -> (mn, name)) =
  RC [Symbol KindLevel (runProperName name) (Link mn)]

type FixityAlias = Qualified (Either (ProperName 'TypeName) (Either Ident (ProperName 'ConstructorName)))

alias :: FixityAlias -> RenderedCode
alias for =
  prefix <> RC [Symbol ns name (Link mn)]
  where
  (ns, name, mn) = unpackFixityAlias for
  prefix = case ns of
    TypeLevel ->
      keywordType <> sp
    _ ->
      mempty

aliasName :: FixityAlias -> Text -> RenderedCode
aliasName for name' =
  let
    (ns, _, _) = unpackFixityAlias for
    unParen = T.tail . T.init
    name = unParen name'
  in
    case ns of
      ValueLevel ->
        ident (Qualified Nothing (Ident name))
      TypeLevel ->
        typeCtor (Qualified Nothing (ProperName name))
      KindLevel ->
        internalError "Kind aliases are not supported"

-- | Converts a FixityAlias into a different representation which is more
-- useful to other functions in this module.
unpackFixityAlias :: FixityAlias -> (Namespace, Text, ContainingModule)
unpackFixityAlias (fromQualified -> (mn, x)) =
  case x of
    -- We add some seemingly superfluous type signatures here just to be extra
    -- sure we are not mixing up our namespaces.
    Left (n :: ProperName 'TypeName) ->
      (TypeLevel, runProperName n, mn)
    Right (Left n) ->
      (ValueLevel, runIdent n, mn)
    Right (Right (n :: ProperName 'ConstructorName)) ->
      (ValueLevel, runProperName n, mn)
