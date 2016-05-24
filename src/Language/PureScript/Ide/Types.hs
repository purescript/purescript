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

{-# LANGUAGE OverloadedStrings          #-}

module Language.PureScript.Ide.Types where

import           Prelude                              ()
import           Prelude.Compat

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Map.Lazy                        as M
import           Data.Maybe                           (maybeToList)
import           Data.Text                            (Text (), pack, unpack)
import qualified Language.PureScript.AST.Declarations as D
import           Language.PureScript.Externs
import qualified Language.PureScript.Errors.JSON      as P
import qualified Language.PureScript.Names            as N
import qualified Language.PureScript as P

import           Text.Parsec
import           Text.Parsec.Text

type Ident = Text
type DeclIdent = Text
type ModuleIdent = Text

data ExternDecl
    -- | A function/value declaration
    = ValueDeclaration Ident P.Type
    | TypeDeclaration (P.ProperName 'P.TypeName) P.Kind
    | TypeSynonymDeclaration (P.ProperName 'P.TypeName) P.Type
    -- | A Dependency onto another Module
    | Dependency
        ModuleIdent  -- name of the dependency
        [Text]       -- explicit imports
        (Maybe Text) -- An eventual qualifier
    -- | A module declaration
    | ModuleDecl
        ModuleIdent -- The modules name
        [DeclIdent] -- The exported identifiers
    -- | A data/newtype declaration
    | DataConstructor
      DeclIdent -- The type name
      (P.ProperName 'P.TypeName)
      P.Type      -- The "type"
    -- | An exported module
    | TypeClassDeclaration (P.ProperName 'P.ClassName)
    | ValueOperator (P.OpName 'P.ValueOpName) Ident P.Precedence P.Associativity
    | TypeOperator (P.OpName 'P.TypeOpName) Ident P.Precedence P.Associativity
    | Export ModuleIdent -- The exported Modules name
    deriving (Show,Eq,Ord)

type Module = (ModuleIdent, [ExternDecl])

data Configuration =
  Configuration
  { confOutputPath :: FilePath
  , confDebug      :: Bool
  }

data PscIdeEnvironment =
  PscIdeEnvironment
  { envStateVar      :: TVar PscIdeState
  , envConfiguration :: Configuration
  }

type PscIde m = (MonadIO m, MonadReader PscIdeEnvironment m)

data PscIdeState =
  PscIdeState
  { pscIdeStateModules       :: M.Map Text [ExternDecl]
  , pscIdeStateExternsFiles  :: M.Map P.ModuleName ExternsFile
  , pscIdeStateCachedRebuild :: Maybe (P.ModuleName, ExternsFile)
  } deriving Show

emptyPscIdeState :: PscIdeState
emptyPscIdeState = PscIdeState M.empty M.empty Nothing

data Match = Match ModuleIdent ExternDecl
               deriving (Show, Eq)

newtype Completion =
  Completion (ModuleIdent, DeclIdent, Text)
  deriving (Show,Eq)

instance ToJSON Completion where
  toJSON (Completion (m,d,t)) =
    object ["module" .= m, "identifier" .= d, "type" .= t]

data ModuleImport =
  ModuleImport
  { importModuleName :: ModuleIdent
  , importType       :: D.ImportDeclarationType
  , importQualifier  :: Maybe Text
  } deriving(Show)

instance Eq ModuleImport where
  mi1 == mi2 =
    importModuleName mi1 == importModuleName mi2
    && importQualifier mi1 == importQualifier mi2

instance ToJSON ModuleImport where
  toJSON (ModuleImport mn D.Implicit qualifier) =
    object $ [ "module" .= mn
             , "importType" .= ("implicit" :: Text)
             ] ++ fmap (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (D.Explicit refs) _) =
    object [ "module" .= mn
           , "importType" .= ("explicit" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)
           ]
  toJSON (ModuleImport mn (D.Hiding refs) _) =
    object [ "module" .= mn
           , "importType" .= ("hiding" :: Text)
           , "identifiers" .= (identifierFromDeclarationRef <$> refs)
           ]

identifierFromDeclarationRef :: D.DeclarationRef -> String
identifierFromDeclarationRef (D.TypeRef name _) = N.runProperName name
identifierFromDeclarationRef (D.ValueRef ident) = N.runIdent ident
identifierFromDeclarationRef (D.TypeClassRef name) = N.runProperName name
identifierFromDeclarationRef _ = ""

data Success =
  CompletionResult [Completion]
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
  | DeclarationResponse Text ModuleIdent DeclIdent Text
  deriving (Show,Eq)

instance FromJSON PursuitResponse where
  parseJSON (Object o) = do
    package <- o .: "package"
    info <- o .: "info"
    (type' :: String) <- info .: "type"
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
  Right (x,y) -> Right (pack x, pack y)
  Left err -> Left (pack (show err))
  where
    parseType :: Parser (String, String)
    parseType = do
      name <- identifier
      _ <- string "::"
      spaces
      type' <- many1 anyChar
      pure (unpack name, type')

    identifier :: Parser Text
    identifier = do
      spaces
      ident <-
        -- necessary for being able to parse the following ((++), concat)
        between (char '(') (char ')') (many1 (noneOf ", )")) <|>
        many1 (noneOf ", )")
      spaces
      pure (pack ident)

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
