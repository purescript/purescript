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

{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Ide.Types where

import           Protolude

import           Control.Concurrent.STM
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Map.Lazy as M
import qualified Language.PureScript as P
import qualified Language.PureScript.Errors.JSON as P

type ModuleIdent = Text
type ModuleMap a = Map P.ModuleName a

data IdeDeclaration
  = IdeDeclValue IdeValue
  | IdeDeclType IdeType
  | IdeDeclTypeSynonym IdeTypeSynonym
  | IdeDeclDataConstructor IdeDataConstructor
  | IdeDeclTypeClass IdeTypeClass
  | IdeDeclValueOperator IdeValueOperator
  | IdeDeclTypeOperator IdeTypeOperator
  | IdeDeclKind (P.ProperName 'P.KindName)
  deriving (Show, Eq, Ord)

data IdeValue = IdeValue
  { _ideValueIdent :: P.Ident
  , _ideValueType  :: P.Type
  } deriving (Show, Eq, Ord)

data IdeType = IdeType
 { _ideTypeName :: P.ProperName 'P.TypeName
 , _ideTypeKind :: P.Kind
 } deriving (Show, Eq, Ord)

data IdeTypeSynonym = IdeTypeSynonym
  { _ideSynonymName :: P.ProperName 'P.TypeName
  , _ideSynonymType :: P.Type
  , _ideSynonymKind :: P.Kind
  } deriving (Show, Eq, Ord)

data IdeDataConstructor = IdeDataConstructor
  { _ideDtorName     :: P.ProperName 'P.ConstructorName
  , _ideDtorTypeName :: P.ProperName 'P.TypeName
  , _ideDtorType     :: P.Type
  } deriving (Show, Eq, Ord)

data IdeTypeClass = IdeTypeClass
  { _ideTCName :: P.ProperName 'P.ClassName
  , _ideTCKind :: P.Kind
  , _ideTCInstances :: [IdeInstance]
  } deriving (Show, Eq, Ord)

data IdeInstance = IdeInstance
  { _ideInstanceModule      :: P.ModuleName
  , _ideInstanceName        :: P.Ident
  , _ideInstanceTypes       :: [P.Type]
  , _ideInstanceConstraints :: Maybe [P.Constraint]
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
makeLenses ''IdeTypeSynonym
makeLenses ''IdeDataConstructor
makeLenses ''IdeTypeClass
makeLenses ''IdeInstance
makeLenses ''IdeValueOperator
makeLenses ''IdeTypeOperator

data IdeDeclarationAnn = IdeDeclarationAnn
  { _idaAnnotation  :: Annotation
  , _idaDeclaration :: IdeDeclaration
  } deriving (Show, Eq, Ord)

data Annotation
  = Annotation
  { _annLocation       :: Maybe P.SourceSpan
  , _annExportedFrom   :: Maybe P.ModuleName
  , _annTypeAnnotation :: Maybe P.Type
  } deriving (Show, Eq, Ord)

makeLenses ''Annotation
makeLenses ''IdeDeclarationAnn

emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing Nothing

type DefinitionSites a = Map IdeNamespaced a
type TypeAnnotations = Map P.Ident P.Type
newtype AstData a = AstData (ModuleMap (DefinitionSites a, TypeAnnotations))
  -- ^ SourceSpans for the definition sites of values and types as well as type
  -- annotations found in a module
  deriving (Show, Eq, Ord, Functor, Foldable)

data IdeLogLevel = LogDebug | LogPerf | LogAll | LogDefault | LogNone
  deriving (Show, Eq)

data IdeConfiguration =
  IdeConfiguration
  { confOutputPath :: FilePath
  , confLogLevel   :: IdeLogLevel
  , confGlobs      :: [FilePath]
  }

data IdeEnvironment =
  IdeEnvironment
  { ideStateVar      :: TVar IdeState
  , ideConfiguration :: IdeConfiguration
  }

type Ide m = (MonadIO m, MonadReader IdeEnvironment m)

data IdeState = IdeState
  { ideFileState     :: IdeFileState
  , ideVolatileState :: IdeVolatileState
  } deriving (Show)

emptyIdeState :: IdeState
emptyIdeState = IdeState emptyFileState emptyVolatileState

emptyFileState :: IdeFileState
emptyFileState = IdeFileState M.empty M.empty

emptyVolatileState :: IdeVolatileState
emptyVolatileState = IdeVolatileState (AstData M.empty) M.empty Nothing


-- | @IdeFileState@ holds data that corresponds 1-to-1 to an entity on the
-- filesystem. Externs correspond to the ExternsFiles the compiler emits into
-- the output folder, and modules are parsed ASTs from source files. This means,
-- that we can update single modules or ExternsFiles inside this state whenever
-- the corresponding entity changes on the file system.
data IdeFileState = IdeFileState
  { fsExterns :: ModuleMap P.ExternsFile
  , fsModules :: ModuleMap (P.Module, FilePath)
  } deriving (Show)

-- | @IdeVolatileState@ is derived from the @IdeFileState@ and needs to be
-- invalidated and refreshed carefully. It holds @AstData@, which is the data we
-- extract from the parsed ASTs, as well as the IdeDeclarations, which contain
-- lots of denormalized data, so they need to fully rebuilt whenever
-- @IdeFileState@ changes. The vsCachedRebuild field can hold a rebuild result
-- with open imports which is used to provide completions for module private
-- declarations
data IdeVolatileState = IdeVolatileState
  { vsAstData       :: AstData P.SourceSpan
  , vsDeclarations  :: ModuleMap [IdeDeclarationAnn]
  , vsCachedRebuild :: Maybe (P.ModuleName, P.ExternsFile)
  } deriving (Show)

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
  , complExportedFrom  :: [P.ModuleName]
  } deriving (Show, Eq, Ord)

instance ToJSON Completion where
  toJSON (Completion {..}) =
    object [ "module" .= complModule
           , "identifier" .= complIdentifier
           , "type" .= complType
           , "expandedType" .= complExpandedType
           , "definedAt" .= complLocation
           , "documentation" .= complDocumentation
           , "exportedFrom" .= map P.runModuleName complExportedFrom
           ]

identifierFromDeclarationRef :: P.DeclarationRef -> Text
identifierFromDeclarationRef (P.TypeRef _ name _) = P.runProperName name
identifierFromDeclarationRef (P.ValueRef _ ident) = P.runIdent ident
identifierFromDeclarationRef (P.TypeClassRef _ name) = P.runProperName name
identifierFromDeclarationRef (P.KindRef _ name) = P.runProperName name
identifierFromDeclarationRef (P.ValueOpRef _ op) = P.showOp op
identifierFromDeclarationRef (P.TypeOpRef _ op) = P.showOp op
identifierFromDeclarationRef _ = ""

data Success =
  CompletionResult [Completion]
  | TextResult Text
  | MultilineTextResult [Text]
  | PursuitResult [PursuitResponse]
  | ImportList (P.ModuleName, [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)])
  | ModuleList [ModuleIdent]
  | RebuildSuccess P.MultipleErrors
  deriving (Show)

encodeSuccess :: (ToJSON a) => a -> Value
encodeSuccess res =
    object ["resultType" .= ("success" :: Text), "result" .= res]

instance ToJSON Success where
  toJSON (CompletionResult cs) = encodeSuccess cs
  toJSON (TextResult t) = encodeSuccess t
  toJSON (MultilineTextResult ts) = encodeSuccess ts
  toJSON (PursuitResult resp) = encodeSuccess resp
  toJSON (ImportList (moduleName, imports)) = object [ "resultType" .= ("success" :: Text)
                                                     , "result" .= object [ "imports" .= map encodeImport imports
                                                                          , "moduleName" .= P.runModuleName moduleName]]
  toJSON (ModuleList modules) = encodeSuccess modules
  toJSON (RebuildSuccess warnings) = encodeSuccess (P.toJSONErrors False P.Warning warnings)

encodeImport :: (P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName) -> Value
encodeImport (P.runModuleName -> mn, importType, map P.runModuleName -> qualifier) = case importType of
  P.Implicit ->
    object $ [ "module" .= mn
             , "importType" .= ("implicit" :: Text)
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)
  P.Explicit refs ->
    object $ [ "module" .= mn
             , "importType" .= ("explicit" :: Text)
             , "identifiers" .= (identifierFromDeclarationRef <$> refs)
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)
  P.Hiding refs ->
    object $ [ "module" .= mn
             , "importType" .= ("hiding" :: Text)
             , "identifiers" .= (identifierFromDeclarationRef <$> refs)
             ] ++ map (\x -> "qualifier" .= x) (maybeToList qualifier)

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

-- | Denotes the different namespaces a name in PureScript can reside in.
data IdeNamespace = IdeNSValue | IdeNSType | IdeNSKind
  deriving (Show, Eq, Ord)

instance FromJSON IdeNamespace where
  parseJSON (String s) = case s of
    "value" -> pure IdeNSValue
    "type"  -> pure IdeNSType
    "kind"  -> pure IdeNSKind
    _       -> mzero
  parseJSON _ = mzero

-- | A name tagged with a namespace
data IdeNamespaced = IdeNamespaced IdeNamespace Text
  deriving (Show, Eq, Ord)
