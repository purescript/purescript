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

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Ide.Types where

import           Protolude hiding (moduleName)

import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Map.Lazy as M
import qualified Language.PureScript as P
import qualified Language.PureScript.Errors.JSON as P
import           Lens.Micro.Platform hiding ((.=))

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
  | IdeDeclModule P.ModuleName
  | IdeDeclKind (P.ProperName 'P.KindName)
  deriving (Show, Eq, Ord, Generic, NFData)

data IdeValue = IdeValue
  { _ideValueIdent :: P.Ident
  , _ideValueType  :: P.SourceType
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeType = IdeType
 { _ideTypeName :: P.ProperName 'P.TypeName
 , _ideTypeKind :: P.SourceKind
 , _ideTypeDtors :: [(P.ProperName 'P.ConstructorName, P.SourceType)]
 } deriving (Show, Eq, Ord, Generic, NFData)

data IdeTypeSynonym = IdeTypeSynonym
  { _ideSynonymName :: P.ProperName 'P.TypeName
  , _ideSynonymType :: P.SourceType
  , _ideSynonymKind :: P.SourceKind
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeDataConstructor = IdeDataConstructor
  { _ideDtorName     :: P.ProperName 'P.ConstructorName
  , _ideDtorTypeName :: P.ProperName 'P.TypeName
  , _ideDtorType     :: P.SourceType
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeTypeClass = IdeTypeClass
  { _ideTCName :: P.ProperName 'P.ClassName
  , _ideTCKind :: P.SourceKind
  , _ideTCInstances :: [IdeInstance]
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeInstance = IdeInstance
  { _ideInstanceModule      :: P.ModuleName
  , _ideInstanceName        :: P.Ident
  , _ideInstanceTypes       :: [P.SourceType]
  , _ideInstanceConstraints :: Maybe [P.SourceConstraint]
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeValueOperator = IdeValueOperator
  { _ideValueOpName          :: P.OpName 'P.ValueOpName
  , _ideValueOpAlias         :: P.Qualified (Either P.Ident (P.ProperName 'P.ConstructorName))
  , _ideValueOpPrecedence    :: P.Precedence
  , _ideValueOpAssociativity :: P.Associativity
  , _ideValueOpType          :: Maybe P.SourceType
  } deriving (Show, Eq, Ord, Generic, NFData)

data IdeTypeOperator = IdeTypeOperator
  { _ideTypeOpName          :: P.OpName 'P.TypeOpName
  , _ideTypeOpAlias         :: P.Qualified (P.ProperName 'P.TypeName)
  , _ideTypeOpPrecedence    :: P.Precedence
  , _ideTypeOpAssociativity :: P.Associativity
  , _ideTypeOpKind          :: Maybe P.SourceKind
  } deriving (Show, Eq, Ord, Generic, NFData)

_IdeDeclValue :: Traversal' IdeDeclaration IdeValue
_IdeDeclValue f (IdeDeclValue x) = map IdeDeclValue (f x)
_IdeDeclValue _ x = pure x

_IdeDeclType :: Traversal' IdeDeclaration IdeType
_IdeDeclType f (IdeDeclType x) = map IdeDeclType (f x)
_IdeDeclType _ x = pure x

_IdeDeclTypeSynonym :: Traversal' IdeDeclaration IdeTypeSynonym
_IdeDeclTypeSynonym f (IdeDeclTypeSynonym x) = map IdeDeclTypeSynonym (f x)
_IdeDeclTypeSynonym _ x = pure x

_IdeDeclDataConstructor :: Traversal' IdeDeclaration IdeDataConstructor
_IdeDeclDataConstructor f (IdeDeclDataConstructor x) = map IdeDeclDataConstructor (f x)
_IdeDeclDataConstructor _ x = pure x

_IdeDeclTypeClass :: Traversal' IdeDeclaration IdeTypeClass
_IdeDeclTypeClass f (IdeDeclTypeClass x) = map IdeDeclTypeClass (f x)
_IdeDeclTypeClass _ x = pure x

_IdeDeclValueOperator :: Traversal' IdeDeclaration IdeValueOperator
_IdeDeclValueOperator f (IdeDeclValueOperator x) = map IdeDeclValueOperator (f x)
_IdeDeclValueOperator _ x = pure x

_IdeDeclTypeOperator :: Traversal' IdeDeclaration IdeTypeOperator
_IdeDeclTypeOperator f (IdeDeclTypeOperator x) = map IdeDeclTypeOperator (f x)
_IdeDeclTypeOperator _ x = pure x

_IdeDeclKind :: Traversal' IdeDeclaration (P.ProperName 'P.KindName)
_IdeDeclKind f (IdeDeclKind x) = map IdeDeclKind (f x)
_IdeDeclKind _ x = pure x

_IdeDeclModule :: Traversal' IdeDeclaration P.ModuleName
_IdeDeclModule f (IdeDeclModule x) = map IdeDeclModule (f x)
_IdeDeclModule _ x = pure x

anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf g p = getAny . getConst . g (Const . Any . p)

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
  } deriving (Show, Eq, Ord, Generic, NFData)

data Annotation
  = Annotation
  { _annLocation       :: Maybe P.SourceSpan
  , _annExportedFrom   :: Maybe P.ModuleName
  , _annTypeAnnotation :: Maybe P.SourceType
  , _annDocumentation  :: Maybe Text
  } deriving (Show, Eq, Ord, Generic, NFData)

makeLenses ''Annotation
makeLenses ''IdeDeclarationAnn

emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing Nothing Nothing

type DefinitionSites a = Map IdeNamespaced a
type TypeAnnotations = Map P.Ident P.SourceType
newtype AstData a = AstData (ModuleMap (DefinitionSites a, TypeAnnotations))
  -- ^ SourceSpans for the definition sites of values and types as well as type
  -- annotations found in a module
  deriving (Show, Eq, Ord, Generic, NFData, Functor, Foldable)

data IdeLogLevel = LogDebug | LogPerf | LogAll | LogDefault | LogNone
  deriving (Show, Eq)

data IdeConfiguration =
  IdeConfiguration
  { confOutputPath :: FilePath
  , confLogLevel   :: IdeLogLevel
  , confGlobs      :: [FilePath]
  , confEditorMode :: Bool
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
  | UsagesResult [P.SourceSpan]
  | MultilineTextResult [Text]
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
  toJSON (UsagesResult ssp) = encodeSuccess ssp
  toJSON (MultilineTextResult ts) = encodeSuccess ts
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

-- | Denotes the different namespaces a name in PureScript can reside in.
data IdeNamespace = IdeNSValue | IdeNSType | IdeNSKind | IdeNSModule
  deriving (Show, Eq, Ord, Generic, NFData)

instance FromJSON IdeNamespace where
  parseJSON (String s) = case s of
    "value" -> pure IdeNSValue
    "type" -> pure IdeNSType
    "kind" -> pure IdeNSKind
    "module" -> pure IdeNSModule
    _       -> mzero
  parseJSON _ = mzero

-- | A name tagged with a namespace
data IdeNamespaced = IdeNamespaced IdeNamespace Text
  deriving (Show, Eq, Ord, Generic, NFData)
