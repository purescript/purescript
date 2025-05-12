-- |
-- Type definitions for psc-ide

{-# language DeriveAnyClass, NoGeneralizedNewtypeDeriving, TemplateHaskell #-}

module Language.PureScript.Ide.Types where

import Protolude hiding (moduleName)

import Control.Concurrent.STM (TVar)
import Control.Lens (Getting, Traversal', makeLenses)
import Control.Monad.Fail (fail)
import Data.Aeson (ToJSON, FromJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.IORef (IORef)
import Data.Time.Clock (UTCTime)
import Data.Map.Lazy qualified as M
import Language.PureScript qualified as P
import Language.PureScript.Errors.JSON qualified as P
import Language.PureScript.Ide.Filter.Declaration (DeclarationType(..))

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
  deriving (Show, Eq, Ord)

data IdeValue = IdeValue
  { _ideValueIdent :: P.Ident
  , _ideValueType :: P.SourceType
  } deriving (Show, Eq, Ord)

data IdeType = IdeType
 { _ideTypeName :: P.ProperName 'P.TypeName
 , _ideTypeKind :: P.SourceType
 , _ideTypeDtors :: [(P.ProperName 'P.ConstructorName, P.SourceType)]
 } deriving (Show, Eq, Ord)

data IdeTypeSynonym = IdeTypeSynonym
  { _ideSynonymName :: P.ProperName 'P.TypeName
  , _ideSynonymType :: P.SourceType
  , _ideSynonymKind :: P.SourceType
  } deriving (Show, Eq, Ord)

data IdeDataConstructor = IdeDataConstructor
  { _ideDtorName :: P.ProperName 'P.ConstructorName
  , _ideDtorTypeName :: P.ProperName 'P.TypeName
  , _ideDtorType :: P.SourceType
  } deriving (Show, Eq, Ord)

data IdeTypeClass = IdeTypeClass
  { _ideTCName :: P.ProperName 'P.ClassName
  , _ideTCKind :: P.SourceType
  , _ideTCInstances :: [IdeInstance]
  } deriving (Show, Eq, Ord)

data IdeInstance = IdeInstance
  { _ideInstanceModule :: P.ModuleName
  , _ideInstanceName :: P.Ident
  , _ideInstanceTypes :: [P.SourceType]
  , _ideInstanceConstraints :: Maybe [P.SourceConstraint]
  } deriving (Show, Eq, Ord)

data IdeValueOperator = IdeValueOperator
  { _ideValueOpName :: P.OpName 'P.ValueOpName
  , _ideValueOpAlias :: P.Qualified (Either P.Ident (P.ProperName 'P.ConstructorName))
  , _ideValueOpPrecedence :: P.Precedence
  , _ideValueOpAssociativity :: P.Associativity
  , _ideValueOpType :: Maybe P.SourceType
  } deriving (Show, Eq, Ord)

data IdeTypeOperator = IdeTypeOperator
  { _ideTypeOpName :: P.OpName 'P.TypeOpName
  , _ideTypeOpAlias :: P.Qualified (P.ProperName 'P.TypeName)
  , _ideTypeOpPrecedence :: P.Precedence
  , _ideTypeOpAssociativity :: P.Associativity
  , _ideTypeOpKind :: Maybe P.SourceType
  } deriving (Show, Eq, Ord)

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
makeLenses ''IdeValueOperator
makeLenses ''IdeTypeOperator

data IdeDeclarationAnn = IdeDeclarationAnn
  { _idaAnnotation :: Annotation
  , _idaDeclaration :: IdeDeclaration
  } deriving (Show, Eq, Ord)

data Annotation
  = Annotation
  { _annLocation :: Maybe P.SourceSpan
  , _annExportedFrom :: Maybe P.ModuleName
  , _annTypeAnnotation :: Maybe P.SourceType
  , _annDocumentation :: Maybe Text
  } deriving (Show, Eq, Ord)

makeLenses ''Annotation
makeLenses ''IdeDeclarationAnn

emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing Nothing Nothing

type DefinitionSites a = Map IdeNamespaced a
type TypeAnnotations = Map P.Ident P.SourceType
newtype AstData a = AstData (ModuleMap (DefinitionSites a, TypeAnnotations))
  -- ^ SourceSpans for the definition sites of values and types as well as type
  -- annotations found in a module
  deriving (Show, Eq, Ord, Functor, Foldable)

data IdeLogLevel = LogDebug | LogPerf | LogAll | LogDefault | LogNone
  deriving (Show, Eq)

data IdeConfiguration =
  IdeConfiguration
  { confOutputPath :: FilePath
  , confLogLevel :: IdeLogLevel
  , confGlobs :: [FilePath]
  , confGlobsFromFile :: Maybe FilePath
  , confGlobsExclude :: [FilePath]
  }

data IdeEnvironment =
  IdeEnvironment
  { ideStateVar :: TVar IdeState
  , ideConfiguration :: IdeConfiguration
  , ideCacheDbTimestamp :: IORef (Maybe UTCTime)
  }

type Ide m = (MonadIO m, MonadReader IdeEnvironment m)

data IdeState = IdeState
  { ideFileState :: IdeFileState
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
  { vsAstData :: AstData P.SourceSpan
  , vsDeclarations :: ModuleMap [IdeDeclarationAnn]
  , vsCachedRebuild :: Maybe (P.ModuleName, P.ExternsFile)
  } deriving (Show)

newtype Match a = Match (P.ModuleName, a)
           deriving (Show, Eq, Functor)

-- | A completion as it gets sent to the editors
data Completion = Completion
  { complModule :: Text
  , complIdentifier :: Text
  , complType :: Text
  , complExpandedType :: Text
  , complLocation :: Maybe P.SourceSpan
  , complDocumentation :: Maybe Text
  , complExportedFrom :: [P.ModuleName]
  , complDeclarationType :: Maybe DeclarationType
  } deriving (Show, Eq, Ord)

instance ToJSON Completion where
  toJSON Completion {..} =
    Aeson.object
      [ "module" .= complModule
      , "identifier" .= complIdentifier
      , "type" .= complType
      , "expandedType" .= complExpandedType
      , "definedAt" .= complLocation
      , "documentation" .= complDocumentation
      , "exportedFrom" .= map P.runModuleName complExportedFrom
      , "declarationType" .= complDeclarationType
      ]

identifierFromDeclarationRef :: P.DeclarationRef -> Text
identifierFromDeclarationRef = \case
  P.TypeRef _ name _ -> P.runProperName name
  P.ValueRef _ ident -> P.runIdent ident
  P.TypeClassRef _ name -> P.runProperName name
  P.ValueOpRef _ op -> P.showOp op
  P.TypeOpRef _ op -> P.showOp op
  _ -> ""

declarationType :: IdeDeclaration -> DeclarationType
declarationType decl = case decl of
  IdeDeclValue _ -> Value
  IdeDeclType _ -> Type
  IdeDeclTypeSynonym _ -> Synonym
  IdeDeclDataConstructor _ -> DataConstructor
  IdeDeclTypeClass _ -> TypeClass
  IdeDeclValueOperator _ -> ValueOperator
  IdeDeclTypeOperator _ -> TypeOperator
  IdeDeclModule _ -> Module
data Success =
  CompletionResult [Completion]
  | TextResult Text
  | UsagesResult [P.SourceSpan]
  | MultilineTextResult [Text]
  | ImportList (P.ModuleName, [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)])
  | ModuleList [ModuleIdent]
  | RebuildSuccess P.MultipleErrors
  deriving (Show)

encodeSuccess :: ToJSON a => a -> Aeson.Value
encodeSuccess res =
  Aeson.object ["resultType" .= ("success" :: Text), "result" .= res]

instance ToJSON Success where
  toJSON = \case
    CompletionResult cs -> encodeSuccess cs
    TextResult t -> encodeSuccess t
    UsagesResult ssp -> encodeSuccess ssp
    MultilineTextResult ts -> encodeSuccess ts
    ImportList (moduleName, imports) ->
      Aeson.object
        [ "resultType" .= ("success" :: Text)
        , "result" .= Aeson.object
            [ "imports" .= map encodeImport imports
            , "moduleName" .= P.runModuleName moduleName
            ]
        ]
    ModuleList modules -> encodeSuccess modules
    RebuildSuccess warnings -> encodeSuccess (P.toJSONErrors False P.Warning [] warnings)

encodeImport :: (P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName) -> Aeson.Value
encodeImport (P.runModuleName -> mn, importType, map P.runModuleName -> qualifier) = case importType of
  P.Implicit ->
    Aeson.object $
      [ "module" .= mn
      , "importType" .= ("implicit" :: Text)
      ] ++ map ("qualifier" .=) (maybeToList qualifier)
  P.Explicit refs ->
    Aeson.object $
      [ "module" .= mn
      , "importType" .= ("explicit" :: Text)
      , "identifiers" .= (identifierFromDeclarationRef <$> refs)
      ] ++ map ("qualifier" .=) (maybeToList qualifier)
  P.Hiding refs ->
    Aeson.object $
      [ "module" .= mn
      , "importType" .= ("hiding" :: Text)
      , "identifiers" .= (identifierFromDeclarationRef <$> refs)
      ] ++ map ("qualifier" .=) (maybeToList qualifier)

-- | Denotes the different namespaces a name in PureScript can reside in.
data IdeNamespace = IdeNSValue | IdeNSType | IdeNSModule
  deriving (Show, Eq, Ord)

instance FromJSON IdeNamespace where
  parseJSON = Aeson.withText "Namespace" $ \case
    "value" -> pure IdeNSValue
    "type" -> pure IdeNSType
    "module" -> pure IdeNSModule
    s -> fail ("Unknown namespace: " <> show s)

-- | A name tagged with a namespace
data IdeNamespaced = IdeNamespaced IdeNamespace Text
  deriving (Show, Eq, Ord)
