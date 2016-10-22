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

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.PureScript.Ide.Types where

import           Protolude

import           Control.Concurrent.STM
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Map.Lazy                       as M
import qualified Language.PureScript                 as P
import qualified Language.PureScript.Errors.JSON     as P
import           Language.PureScript.Ide.Conversions

type ModuleIdent = Text

data IdeDeclaration
  = IdeDeclValue IdeValue
  | IdeDeclType IdeType
  | IdeDeclTypeSynonym IdeSynonym
  | IdeDeclDataConstructor IdeDataConstructor
  | IdeDeclTypeClass (P.ProperName 'P.ClassName)
  | IdeDeclValueOperator IdeValueOperator
  | IdeDeclTypeOperator IdeTypeOperator
  deriving (Show, Eq, Ord)

data IdeValue = IdeValue
  { _ideValueIdent :: P.Ident
  , _ideValueType  :: P.Type
  } deriving (Show, Eq, Ord)

data IdeType = IdeType
 { _ideTypeName :: P.ProperName 'P.TypeName
 , _ideTypeKind :: P.Kind
 } deriving (Show, Eq, Ord)

data IdeSynonym = IdeSynonym
  { _ideSynonymName :: P.ProperName 'P.TypeName
  , _ideSynonymType :: P.Type
  } deriving (Show, Eq, Ord)

data IdeDataConstructor = IdeDataConstructor
  { _ideDtorName     :: P.ProperName 'P.ConstructorName
  , _ideDtorTypeName :: P.ProperName 'P.TypeName
  , _ideDtorType     :: P.Type
  } deriving (Show, Eq, Ord)

data IdeValueOperator = IdeValueOperator
  { _ideValueOpName          :: P.OpName 'P.ValueOpName
  , _ideValueOpAlias         :: P.Qualified (Either P.Ident (P.ProperName 'P.ConstructorName))
  , _ideValueOpPrecedence    :: P.Precedence
  , _ideValueOpAssociativity :: P.Associativity
  , _ideValueOpType          :: Maybe P.Type
  } deriving (Show, Eq, Ord)

data IdeTypeOperator = IdeTypeOperator
  { _ideTypeOpName          :: P.OpName 'P.TypeOpName
  , _ideTypeOpAlias         :: P.Qualified (P.ProperName 'P.TypeName)
  , _ideTypeOpPrecedence    :: P.Precedence
  , _ideTypeOpAssociativity :: P.Associativity
  , _ideTypeOpKind          :: Maybe P.Kind
  } deriving (Show, Eq, Ord)

makePrisms ''IdeDeclaration
makeLenses ''IdeValue
makeLenses ''IdeType
makeLenses ''IdeSynonym
makeLenses ''IdeDataConstructor
makeLenses ''IdeValueOperator
makeLenses ''IdeTypeOperator

data IdeDeclarationAnn = IdeDeclarationAnn
  { _idaAnnotation  :: Annotation
  , _idaDeclaration :: IdeDeclaration
  } deriving (Show, Eq, Ord)

data Annotation
  = Annotation
  { annLocation       :: Maybe P.SourceSpan
  , annExportedFrom   :: Maybe P.ModuleName
  , annTypeAnnotation :: Maybe P.Type
  } deriving (Show, Eq, Ord)

makeLenses ''IdeDeclarationAnn

emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing Nothing

type Module = (P.ModuleName, [IdeDeclarationAnn])

type DefinitionSites a = Map (Either Text Text) a
type TypeAnnotations = Map P.Ident P.Type
newtype AstData a = AstData (Map P.ModuleName (DefinitionSites a, TypeAnnotations))
  -- ^ SourceSpans for the definition sites of Values and Types aswell as type
  -- annotations found in a module
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
  { s3Declarations  :: M.Map P.ModuleName [IdeDeclarationAnn]
  , s3CachedRebuild :: Maybe (P.ModuleName, P.ExternsFile)
  }

newtype Match a = Match (P.ModuleName, a)
           deriving (Show, Eq, Functor)

-- | A completion as it gets sent to the editors
data Completion = Completion
  { complModule        :: Text
  , complIdentifier    :: Text
  , complType          :: Text
  , complExpandedType  :: Text
  , complLocation      :: Maybe P.SourceSpan
  , complDocumentation :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON Completion where
  toJSON (Completion {..}) =
    object [ "module" .= complModule
           , "identifier" .= complIdentifier
           , "type" .= complType
           , "expandedType" .= complExpandedType
           , "definedAt" .= complLocation
           , "documentation" .= complDocumentation
           ]

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
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (P.Explicit refs) qualifier) =
    object $ [ "module" .= mn
             , "importType" .= ("explicit" :: Text)
             , "identifiers" .= (identifierFromDeclarationRef <$> refs)
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)
  toJSON (ModuleImport mn (P.Hiding refs) qualifier) =
    object $ [ "module" .= mn
             , "importType" .= ("hiding" :: Text)
             , "identifiers" .= (identifierFromDeclarationRef <$> refs)
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)

identifierFromDeclarationRef :: P.DeclarationRef -> Text
identifierFromDeclarationRef (P.TypeRef name _) = runProperNameT name
identifierFromDeclarationRef (P.ValueRef ident) = runIdentT ident
identifierFromDeclarationRef (P.TypeClassRef name) = runProperNameT name
identifierFromDeclarationRef _ = ""

data Success =
  CompletionResult [Completion]
  | TextResult Text
  | MultilineTextResult [Text]
  | PursuitResult [PursuitResponse]
  | ImportList [ModuleImport]
  | ModuleList [ModuleIdent]
  | RebuildSuccess [P.JSONError]
  deriving (Show, Eq)

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
  -- | A Pursuit Response for a declaration. Consist of the declaration's
  -- module, name, package, type summary text
  | DeclarationResponse Text ModuleIdent Text (Maybe Text) Text
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
        ident <- info .: "title"
        (text :: Text) <- o .: "text"
        typ <- info .:? "typeText"
        pure (DeclarationResponse moduleName ident package typ text)
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON PursuitResponse where
  toJSON (ModuleResponse name package) =
    object ["module" .= name, "package" .= package]
  toJSON (DeclarationResponse module' ident package type' text) =
    object
      [ "module"  .= module'
      , "ident"   .= ident
      , "type"    .= type'
      , "package" .= package
      , "text"    .= text
      ]
