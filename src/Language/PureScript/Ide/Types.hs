-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Types
-- Description : Type definitions for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Type definitions for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable    #-}

module Language.PureScript.Ide.Types where

import           Protolude

import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Map.Lazy                        as M
import qualified Language.PureScript.Errors.JSON      as P
import qualified Language.PureScript as P
import           Language.PureScript.Ide.Conversions
import           Text.Parsec as Parsec
import           Text.Parsec.Text

type ModuleIdent = Text

data IdeDeclaration
  = IdeValue P.Ident P.Type
  | IdeType (P.ProperName 'P.TypeName) P.Kind
  | IdeTypeSynonym (P.ProperName 'P.TypeName) P.Type
  | IdeDataConstructor (P.ProperName 'P.ConstructorName) (P.ProperName 'P.TypeName) P.Type
  | IdeTypeClass (P.ProperName 'P.ClassName)
  | IdeValueOperator (P.OpName 'P.ValueOpName) Text P.Precedence P.Associativity
  | IdeTypeOperator (P.OpName 'P.TypeOpName) Text P.Precedence P.Associativity
  deriving (Show, Eq, Ord)

data IdeDeclarationAnn = IdeDeclarationAnn Annotation IdeDeclaration
  deriving (Show, Eq, Ord)

data Annotation
  = Annotation
  { annLocation     :: Maybe P.SourceSpan
  , annExportedFrom :: Maybe P.ModuleName
  } deriving (Show, Eq, Ord)

emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing

type Module = (P.ModuleName, [IdeDeclarationAnn])

newtype AstData a =
  AstData (Map P.ModuleName (Map (Either Text Text) a))
  deriving (Show, Eq, Ord, Functor, Foldable)

data Configuration =
  Configuration
  { confOutputPath :: FilePath
  , confDebug      :: Bool
  , confGlobs      :: [FilePath]
  }

data IdeEnvironment =
  IdeEnvironment
  { ideStateVar      :: TVar IdeState
  , ideConfiguration :: Configuration
  }

type Ide m = (MonadIO m, MonadReader IdeEnvironment m)

data IdeState = IdeState
  { ideStage1 :: Stage1
  , ideStage2 :: Stage2
  , ideStage3 :: Stage3
  }

emptyIdeState :: IdeState
emptyIdeState = IdeState emptyStage1 emptyStage2 emptyStage3

emptyStage1 :: Stage1
emptyStage1 = Stage1 M.empty M.empty

emptyStage2 :: Stage2
emptyStage2 = Stage2 (AstData M.empty)

emptyStage3 :: Stage3
emptyStage3 = Stage3 M.empty Nothing

data Stage1 = Stage1
  { s1Externs :: M.Map P.ModuleName P.ExternsFile
  , s1Modules :: M.Map P.ModuleName (P.Module, FilePath)
  }

data Stage2 = Stage2
  { s2AstData :: AstData P.SourceSpan
  }

data Stage3 = Stage3
  { s3Declarations :: M.Map P.ModuleName [IdeDeclarationAnn]
  , s3CachedRebuild :: Maybe (P.ModuleName, P.ExternsFile)
  }

newtype Match a = Match (P.ModuleName, a)
           deriving (Show, Eq, Functor)

newtype Completion =
  Completion (Text, Text, Text)
  deriving (Show,Eq)

newtype Info =
  Info (Text, Text, Text, Maybe P.SourceSpan)
  deriving (Show,Eq)

instance ToJSON Info where
  toJSON (Info (m, d, t, sourceSpan)) =
    object ["module" .= m, "identifier" .= d, "type" .= t, "definedAt" .= sourceSpan]

instance ToJSON Completion where
  toJSON (Completion (m, d, t)) =
    object ["module" .= m, "identifier" .= d, "type" .= t]

data ModuleImport =
  ModuleImport
  { importModuleName :: ModuleIdent
  , importType       :: P.ImportDeclarationType
  , importQualifier  :: Maybe Text
  } deriving(Show)

instance Eq ModuleImport where
  mi1 == mi2 =
    importModuleName mi1 == importModuleName mi2
    && importQualifier mi1 == importQualifier mi2

instance ToJSON ModuleImport where
  toJSON (ModuleImport mn P.Implicit qualifier) =
    object $ [ "module" .= mn
             , "importType" .= ("implicit" :: Text)
             ] ++ fmap (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (P.Explicit refs) _) =
    object [ "module" .= mn
           , "importType" .= ("explicit" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)
           ]
  toJSON (ModuleImport mn (P.Hiding refs) _) =
    object [ "module" .= mn
           , "importType" .= ("hiding" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)
           ]

identifierFromDeclarationRef :: P.DeclarationRef -> Text
identifierFromDeclarationRef (P.TypeRef name _) = runProperNameT name
identifierFromDeclarationRef (P.ValueRef ident) = runIdentT ident
identifierFromDeclarationRef (P.TypeClassRef name) = runProperNameT name
identifierFromDeclarationRef _ = ""

data Success =
  CompletionResult [Completion]
  | InfoResult [Info]
  | TextResult Text
  | MultilineTextResult [Text]
  | PursuitResult [PursuitResponse]
  | ImportList [ModuleImport]
  | ModuleList [ModuleIdent]
  | RebuildSuccess [P.JSONError]
  deriving(Show, Eq)

encodeSuccess :: (ToJSON a) => a -> Value
encodeSuccess res =
    object ["resultType" .= ("success" :: Text), "result" .= res]

instance ToJSON Success where
  toJSON (CompletionResult cs) = encodeSuccess cs
  toJSON (InfoResult i) = encodeSuccess i
  toJSON (TextResult t) = encodeSuccess t
  toJSON (MultilineTextResult ts) = encodeSuccess ts
  toJSON (PursuitResult resp) = encodeSuccess resp
  toJSON (ImportList decls) = encodeSuccess decls
  toJSON (ModuleList modules) = encodeSuccess modules
  toJSON (RebuildSuccess modules) = encodeSuccess modules

newtype PursuitQuery = PursuitQuery Text
                     deriving (Show, Eq)

data PursuitSearchType = Package | Identifier
                       deriving (Show, Eq)

instance FromJSON PursuitSearchType where
  parseJSON (String t) = case t of
    "package"    -> pure Package
    "completion" -> pure Identifier
    _            -> mzero
  parseJSON _ = mzero

instance FromJSON PursuitQuery where
  parseJSON o = PursuitQuery <$> parseJSON o

data PursuitResponse =
  -- | A Pursuit Response for a module. Consists of the modules name and the
  -- package it belongs to
  ModuleResponse ModuleIdent Text
  -- | A Pursuit Response for a declaration. Consist of the declarations type,
  -- module, name and package
  | DeclarationResponse Text ModuleIdent Text Text
  deriving (Show,Eq)

instance FromJSON PursuitResponse where
  parseJSON (Object o) = do
    package <- o .: "package"
    info <- o .: "info"
    (type' :: Text) <- info .: "type"
    case type' of
      "module" -> do
        name <- info .: "module"
        pure (ModuleResponse name package)
      "declaration" -> do
        moduleName <- info .: "module"
        Right (ident, declType) <- typeParse <$> o .: "text"
        pure (DeclarationResponse declType moduleName ident package)
      _ -> mzero
  parseJSON _ = mzero


typeParse :: Text -> Either Text (Text, Text)
typeParse t = case parse parseType "" t of
  Right (x,y) -> Right (x, y)
  Left err -> Left (show err)
  where
    parseType :: Parser (Text, Text)
    parseType = do
      name <- identifier
      _ <- string "::"
      spaces
      type' <- many1 anyChar
      pure (name, toS type')

    identifier :: Parser Text
    identifier = do
      spaces
      ident <-
        -- necessary for being able to parse the following ((++), concat)
        between (char '(') (char ')') (many1 (noneOf ", )")) Parsec.<|>
        many1 (noneOf ", )")
      spaces
      pure (toS ident)

instance ToJSON PursuitResponse where
  toJSON (ModuleResponse name package) =
    object ["module" .= name, "package" .= package]
  toJSON (DeclarationResponse module' ident type' package) =
    object
      [ "module"  .= module'
      , "ident"   .= ident
      , "type"    .= type'
      , "package" .= package
      ]
