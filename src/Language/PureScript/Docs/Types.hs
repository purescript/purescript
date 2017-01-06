module Language.PureScript.Docs.Types
  ( module Language.PureScript.Docs.Types
  , module ReExports
  )
  where

import Prelude.Compat

import Control.Arrow (first, (***))
import Control.Monad (when)
import Control.Monad.Error.Class (catchError)

import Data.Aeson ((.=))
import Data.Aeson.BetterErrors
import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft, isRight)
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as TimeFormat
import Data.Version
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.PureScript as P

import Text.ParserCombinators.ReadP (readP_to_S)

import Web.Bower.PackageMeta hiding (Version, displayError)

import Language.PureScript.Docs.RenderedCode as ReExports
  (RenderedCode, asRenderedCode,
   ContainingModule(..), asContainingModule,
   RenderedCodeElement(..), asRenderedCodeElement)

--------------------
-- Types

data Package a = Package
  { pkgMeta                 :: PackageMeta
  , pkgVersion              :: Version
  , pkgVersionTag           :: Text
  -- TODO: When this field was introduced, it was given the Maybe type for the
  -- sake of backwards compatibility, as older JSON blobs will not include the
  -- field. It should eventually be changed to just UTCTime.
  , pkgTagTime              :: Maybe UTCTime
  , pkgModules              :: [Module]
  , pkgBookmarks            :: [Bookmark]
  , pkgResolvedDependencies :: [(PackageName, Version)]
  , pkgGithub               :: (GithubUser, GithubRepo)
  , pkgUploader             :: a
  , pkgCompilerVersion      :: Version
    -- ^ The version of the PureScript compiler which was used to generate
    -- this data. We store this in order to reject packages which are too old.
  }
  deriving (Show, Eq, Ord)

data NotYetKnown = NotYetKnown
  deriving (Show, Eq, Ord)

type UploadedPackage = Package NotYetKnown
type VerifiedPackage = Package GithubUser

verifyPackage :: GithubUser -> UploadedPackage -> VerifiedPackage
verifyPackage verifiedUser Package{..} =
  Package pkgMeta
          pkgVersion
          pkgVersionTag
          pkgTagTime
          pkgModules
          pkgBookmarks
          pkgResolvedDependencies
          pkgGithub
          verifiedUser
          pkgCompilerVersion

packageName :: Package a -> PackageName
packageName = bowerName . pkgMeta

-- |
-- The time format used for serializing package tag times in the JSON format.
-- This is the ISO 8601 date format which includes a time and a timezone.
--
jsonTimeFormat :: String
jsonTimeFormat = "%Y-%m-%dT%H:%M:%S%z"

-- |
-- Convenience function for formatting a time in the format expected by this
-- module.
--
formatTime :: UTCTime -> String
formatTime =
  TimeFormat.formatTime TimeFormat.defaultTimeLocale jsonTimeFormat

-- |
-- Convenience function for parsing a time in the format expected by this
-- module.
--
parseTime :: String -> Maybe UTCTime
parseTime =
  TimeFormat.parseTimeM False TimeFormat.defaultTimeLocale jsonTimeFormat

data Module = Module
  { modName         :: P.ModuleName
  , modComments     :: Maybe Text
  , modDeclarations :: [Declaration]
  -- Re-exported values from other modules
  , modReExports    :: [(InPackage P.ModuleName, [Declaration])]
  }
  deriving (Show, Eq, Ord)

data Declaration = Declaration
  { declTitle      :: Text
  , declComments   :: Maybe Text
  , declSourceSpan :: Maybe P.SourceSpan
  , declChildren   :: [ChildDeclaration]
  , declInfo       :: DeclarationInfo
  }
  deriving (Show, Eq, Ord)

-- |
-- A value of this type contains information that is specific to a particular
-- kind of declaration (as opposed to information which exists in all kinds of
-- declarations, which goes into the 'Declaration' type directly).
--
-- Many of the constructors are very similar to their equivalents in the real
-- PureScript AST, except that they have their name elided, since this is
-- already available via the rdTitle field of 'Declaration'.
--
data DeclarationInfo
  -- |
  -- A value declaration, with its type.
  --
  = ValueDeclaration P.Type

  -- |
  -- A data/newtype declaration, with the kind of declaration (data or
  -- newtype) and its type arguments. Constructors are represented as child
  -- declarations.
  --
  | DataDeclaration P.DataDeclType [(Text, Maybe P.Kind)]

  -- |
  -- A data type foreign import, with its kind.
  --
  | ExternDataDeclaration P.Kind

  -- |
  -- A type synonym, with its type arguments and its type.
  --
  | TypeSynonymDeclaration [(Text, Maybe P.Kind)] P.Type

  -- |
  -- A type class, with its type arguments, its superclasses and functional
  -- dependencies. Instances and members are represented as child declarations.
  --
  | TypeClassDeclaration [(Text, Maybe P.Kind)] [P.Constraint] [([Text], [Text])]

  -- |
  -- An operator alias declaration, with the member the alias is for and the
  -- operator's fixity.
  --
  | AliasDeclaration P.Fixity FixityAlias

  -- |
  -- A kind declaration
  --
  | ExternKindDeclaration
  deriving (Show, Eq, Ord)

convertFundepsToStrings :: [(Text, Maybe P.Kind)] -> [P.FunctionalDependency] -> [([Text], [Text])]
convertFundepsToStrings args fundeps =
  map (\(P.FunctionalDependency from to) -> toArgs from to) fundeps
  where
  argsVec = V.fromList (map fst args)
  getArg i =
    fromMaybe
      (P.internalError $ unlines
        [ "convertDeclaration: Functional dependency index"
        , show i
        , "is bigger than arguments list"
        , show (map fst args)
        , "Functional dependencies are"
        , show fundeps
        ]
      ) $ argsVec V.!? i
  toArgs from to = (map getArg from, map getArg to)

type FixityAlias = P.Qualified (Either (P.ProperName 'P.TypeName) (Either P.Ident (P.ProperName 'P.ConstructorName)))

declInfoToString :: DeclarationInfo -> Text
declInfoToString (ValueDeclaration _) = "value"
declInfoToString (DataDeclaration _ _) = "data"
declInfoToString (ExternDataDeclaration _) = "externData"
declInfoToString (TypeSynonymDeclaration _ _) = "typeSynonym"
declInfoToString (TypeClassDeclaration _ _ _) = "typeClass"
declInfoToString (AliasDeclaration _ _) = "alias"
declInfoToString ExternKindDeclaration = "kind"

isTypeClass :: Declaration -> Bool
isTypeClass Declaration{..} =
  case declInfo of
    TypeClassDeclaration{} -> True
    _ -> False

isValue :: Declaration -> Bool
isValue Declaration{..} =
  case declInfo of
    ValueDeclaration{} -> True
    _ -> False

isType :: Declaration ->  Bool
isType Declaration{..} =
  case declInfo of
    TypeSynonymDeclaration{} -> True
    DataDeclaration{} -> True
    ExternDataDeclaration{} -> True
    _ -> False

isValueAlias :: Declaration -> Bool
isValueAlias Declaration{..} =
  case declInfo of
    AliasDeclaration _ (P.Qualified _ d) -> isRight d
    _ -> False

isTypeAlias :: Declaration -> Bool
isTypeAlias Declaration{..} =
  case declInfo of
    AliasDeclaration _ (P.Qualified _ d) -> isLeft d
    _ -> False

isKind :: Declaration -> Bool
isKind Declaration{..} =
  case declInfo of
    ExternKindDeclaration{} -> True
    _ -> False

-- | Discard any children which do not satisfy the given predicate.
filterChildren :: (ChildDeclaration -> Bool) -> Declaration -> Declaration
filterChildren p decl =
  decl { declChildren = filter p (declChildren decl) }

data ChildDeclaration = ChildDeclaration
  { cdeclTitle      :: Text
  , cdeclComments   :: Maybe Text
  , cdeclSourceSpan :: Maybe P.SourceSpan
  , cdeclInfo       :: ChildDeclarationInfo
  }
  deriving (Show, Eq, Ord)

data ChildDeclarationInfo
  -- |
  -- A type instance declaration, with its dependencies and its type.
  --
  = ChildInstance [P.Constraint] P.Type

  -- |
  -- A data constructor, with its type arguments.
  --
  | ChildDataConstructor [P.Type]

  -- |
  -- A type class member, with its type. Note that the type does not include
  -- the type class constraint; this may be added manually if desired. For
  -- example, `pure` from `Applicative` would be `forall a. a -> f a`.
  --
  | ChildTypeClassMember P.Type
  deriving (Show, Eq, Ord)

childDeclInfoToString :: ChildDeclarationInfo -> Text
childDeclInfoToString (ChildInstance _ _)      = "instance"
childDeclInfoToString (ChildDataConstructor _) = "dataConstructor"
childDeclInfoToString (ChildTypeClassMember _) = "typeClassMember"

isTypeClassMember :: ChildDeclaration -> Bool
isTypeClassMember ChildDeclaration{..} =
  case cdeclInfo of
    ChildTypeClassMember{} -> True
    _ -> False

isDataConstructor :: ChildDeclaration -> Bool
isDataConstructor ChildDeclaration{..} =
  case cdeclInfo of
    ChildDataConstructor{} -> True
    _ -> False

newtype GithubUser
  = GithubUser { runGithubUser :: Text }
  deriving (Show, Eq, Ord)

newtype GithubRepo
  = GithubRepo { runGithubRepo :: Text }
  deriving (Show, Eq, Ord)

data PackageError
  = CompilerTooOld Version Version
      -- ^ Minimum allowable version for generating data with the current
      -- parser, and actual version used.
  | ErrorInPackageMeta BowerError
  | InvalidVersion
  | InvalidDeclarationType Text
  | InvalidChildDeclarationType Text
  | InvalidFixity
  | InvalidKind Text
  | InvalidDataDeclType Text
  | InvalidTime
  deriving (Show, Eq, Ord)

type Bookmark = InPackage (P.ModuleName, Text)

data InPackage a
  = Local a
  | FromDep PackageName a
  deriving (Show, Eq, Ord)

instance Functor InPackage where
  fmap f (Local x) = Local (f x)
  fmap f (FromDep pkgName x) = FromDep pkgName (f x)

takeLocal :: InPackage a -> Maybe a
takeLocal (Local a) = Just a
takeLocal _ = Nothing

takeLocals :: [InPackage a] -> [a]
takeLocals = mapMaybe takeLocal

ignorePackage :: InPackage a -> a
ignorePackage (Local x) = x
ignorePackage (FromDep _ x) = x

----------------------
-- Parsing

parseUploadedPackage :: Version -> ByteString -> Either (ParseError PackageError) UploadedPackage
parseUploadedPackage minVersion = parse $ asUploadedPackage minVersion

parseVerifiedPackage :: Version -> ByteString -> Either (ParseError PackageError) VerifiedPackage
parseVerifiedPackage minVersion = parse $ asVerifiedPackage minVersion

asPackage :: Version -> (forall e. Parse e a) -> Parse PackageError (Package a)
asPackage minimumVersion uploader = do
  -- If the compilerVersion key is missing, we can be sure that it was produced
  -- with 0.7.0.0, since that is the only released version that included the
  -- psc-publish tool before this key was added.
  compilerVersion <- keyOrDefault "compilerVersion" (Version [0,7,0,0] []) asVersion
  when (compilerVersion < minimumVersion)
    (throwCustomError $ CompilerTooOld minimumVersion compilerVersion)

  Package <$> key "packageMeta" asPackageMeta .! ErrorInPackageMeta
          <*> key "version" asVersion
          <*> key "versionTag" asText
          <*> keyMay "tagTime" (withString parseTimeEither)
          <*> key "modules" (eachInArray asModule)
          <*> key "bookmarks" asBookmarks .! ErrorInPackageMeta
          <*> key "resolvedDependencies" asResolvedDependencies
          <*> key "github" asGithub
          <*> key "uploader" uploader
          <*> pure compilerVersion

parseTimeEither :: String -> Either PackageError UTCTime
parseTimeEither =
  maybe (Left InvalidTime) Right . parseTime

asUploadedPackage :: Version -> Parse PackageError UploadedPackage
asUploadedPackage minVersion = asPackage minVersion asNotYetKnown

asNotYetKnown :: Parse e NotYetKnown
asNotYetKnown = NotYetKnown <$ asNull

instance A.FromJSON NotYetKnown where
  parseJSON = toAesonParser' asNotYetKnown

asVerifiedPackage :: Version -> Parse PackageError VerifiedPackage
asVerifiedPackage minVersion = asPackage minVersion asGithubUser

displayPackageError :: PackageError -> Text
displayPackageError e = case e of
  CompilerTooOld minV usedV ->
    "Expecting data produced by at least version " <> T.pack (showVersion minV)
    <> " of the compiler, but it appears that " <> T.pack (showVersion usedV)
    <> " was used."
  ErrorInPackageMeta err ->
    "Error in package metadata: " <> showBowerError err
  InvalidVersion ->
    "Invalid version"
  InvalidDeclarationType str ->
    "Invalid declaration type: \"" <> str <> "\""
  InvalidChildDeclarationType str ->
    "Invalid child declaration type: \"" <> str <> "\""
  InvalidFixity ->
    "Invalid fixity"
  InvalidKind str ->
    "Invalid kind: \"" <> str <> "\""
  InvalidDataDeclType str ->
    "Invalid data declaration type: \"" <> str <> "\""
  InvalidTime ->
    "Invalid time"

instance A.FromJSON a => A.FromJSON (Package a) where
  parseJSON = toAesonParser displayPackageError
                            (asPackage (Version [0,0,0,0] []) fromAesonParser)

asGithubUser :: Parse e GithubUser
asGithubUser = GithubUser <$> asText

instance A.FromJSON GithubUser where
  parseJSON = toAesonParser' asGithubUser

asVersion :: Parse PackageError Version
asVersion = withString (maybe (Left InvalidVersion) Right . parseVersion')

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just vers
    _            -> Nothing

asModule :: Parse PackageError Module
asModule =
  Module <$> key "name" (P.moduleNameFromString <$> asText)
         <*> key "comments" (perhaps asText)
         <*> key "declarations" (eachInArray asDeclaration)
         <*> key "reExports" (eachInArray asReExport)

asDeclaration :: Parse PackageError Declaration
asDeclaration =
  Declaration <$> key "title" asText
              <*> key "comments" (perhaps asText)
              <*> key "sourceSpan" (perhaps asSourceSpan)
              <*> key "children" (eachInArray asChildDeclaration)
              <*> key "info" asDeclarationInfo

asReExport :: Parse PackageError (InPackage P.ModuleName, [Declaration])
asReExport =
  (,) <$> key "moduleName" asReExportModuleName
      <*> key "declarations" (eachInArray asDeclaration)
  where
  -- This is to preserve backwards compatibility with 0.10.3 and earlier versions
  -- of the compiler, where the modReExports field had the type
  -- [(P.ModuleName, [Declaration])]. This should eventually be removed,
  -- possibly at the same time as the next breaking change to this JSON format.
  asReExportModuleName :: Parse PackageError (InPackage P.ModuleName)
  asReExportModuleName =
    asInPackage fromAesonParser .! ErrorInPackageMeta
    <|> fmap Local fromAesonParser

  (<|>) p q = catchError p (const q)

asInPackage :: Parse BowerError a -> Parse BowerError (InPackage a)
asInPackage inner =
  build <$> key "package" (perhaps (withText parsePackageName))
        <*> key "item" inner
  where
  build Nothing = Local
  build (Just pn) = FromDep pn

asFixity :: Parse PackageError P.Fixity
asFixity =
  P.Fixity <$> key "associativity" asAssociativity
           <*> key "precedence" asIntegral

asFixityAlias :: Parse PackageError FixityAlias
asFixityAlias = fromAesonParser

parseAssociativity :: String -> Maybe P.Associativity
parseAssociativity str = case str of
  "infix"  -> Just P.Infix
  "infixl" -> Just P.Infixl
  "infixr" -> Just P.Infixr
  _        -> Nothing

asAssociativity :: Parse PackageError P.Associativity
asAssociativity = withString (maybe (Left InvalidFixity) Right . parseAssociativity)

asDeclarationInfo :: Parse PackageError DeclarationInfo
asDeclarationInfo = do
  ty <- key "declType" asText
  case ty of
    "value" ->
      ValueDeclaration <$> key "type" asType
    "data" ->
      DataDeclaration <$> key "dataDeclType" asDataDeclType
                      <*> key "typeArguments" asTypeArguments
    "externData" ->
      ExternDataDeclaration <$> key "kind" asKind
    "typeSynonym" ->
      TypeSynonymDeclaration <$> key "arguments" asTypeArguments
                             <*> key "type" asType
    "typeClass" ->
      TypeClassDeclaration <$> key "arguments" asTypeArguments
                           <*> key "superclasses" (eachInArray asConstraint)
                           <*> keyOrDefault "fundeps" [] asFunDeps
    "alias" ->
      AliasDeclaration <$> key "fixity" asFixity
                       <*> key "alias" asFixityAlias
    "kind" ->
      pure ExternKindDeclaration
    other ->
      throwCustomError (InvalidDeclarationType other)

asTypeArguments :: Parse PackageError [(Text, Maybe P.Kind)]
asTypeArguments = eachInArray asTypeArgument
  where
  asTypeArgument = (,) <$> nth 0 asText <*> nth 1 (perhaps asKind)

asKind :: Parse PackageError P.Kind
asKind = P.kindFromJSON .! InvalidKind

asType :: Parse e P.Type
asType = fromAesonParser

asFunDeps :: Parse PackageError [([Text], [Text])]
asFunDeps = eachInArray asFunDep
  where
  asFunDep = (,) <$> nth 0 (eachInArray asText) <*> nth 1 (eachInArray asText)

asDataDeclType :: Parse PackageError P.DataDeclType
asDataDeclType =
  withText $ \s -> case s of
    "data"    -> Right P.Data
    "newtype" -> Right P.Newtype
    other     -> Left (InvalidDataDeclType other)

asChildDeclaration :: Parse PackageError ChildDeclaration
asChildDeclaration =
  ChildDeclaration <$> key "title" asText
                           <*> key "comments" (perhaps asText)
                           <*> key "sourceSpan" (perhaps asSourceSpan)
                           <*> key "info" asChildDeclarationInfo

asChildDeclarationInfo :: Parse PackageError ChildDeclarationInfo
asChildDeclarationInfo = do
  ty <- key "declType" asText
  case ty of
    "instance" ->
      ChildInstance <$> key "dependencies" (eachInArray asConstraint)
                    <*> key "type" asType
    "dataConstructor" ->
      ChildDataConstructor <$> key "arguments" (eachInArray asType)
    "typeClassMember" ->
      ChildTypeClassMember <$> key "type" asType
    other ->
      throwCustomError $ InvalidChildDeclarationType other

asSourcePos :: Parse e P.SourcePos
asSourcePos = P.SourcePos <$> nth 0 asIntegral
                          <*> nth 1 asIntegral

asConstraint :: Parse PackageError P.Constraint
asConstraint = P.Constraint <$> key "constraintClass" asQualifiedProperName
                            <*> key "constraintArgs" (eachInArray asType)
                            <*> pure Nothing

asQualifiedProperName :: Parse e (P.Qualified (P.ProperName a))
asQualifiedProperName = fromAesonParser

asQualifiedIdent :: Parse e (P.Qualified P.Ident)
asQualifiedIdent = fromAesonParser

asBookmarks :: Parse BowerError [Bookmark]
asBookmarks = eachInArray asBookmark

asBookmark :: Parse BowerError Bookmark
asBookmark =
  asInPackage ((,) <$> nth 0 (P.moduleNameFromString <$> asText)
                   <*> nth 1 asText)

asResolvedDependencies :: Parse PackageError [(PackageName, Version)]
asResolvedDependencies =
  eachInObjectWithKey (mapLeft ErrorInPackageMeta . parsePackageName) asVersion
  where
  mapLeft f (Left x) = Left (f x)
  mapLeft _ (Right x) = Right x

asGithub :: Parse e (GithubUser, GithubRepo)
asGithub = (,) <$> nth 0 (GithubUser <$> asText)
               <*> nth 1 (GithubRepo <$> asText)

asSourceSpan :: Parse e P.SourceSpan
asSourceSpan = P.SourceSpan <$> key "name" asString
                            <*> key "start" asSourcePos
                            <*> key "end" asSourcePos

---------------------
-- ToJSON instances

instance A.ToJSON a => A.ToJSON (Package a) where
  toJSON Package{..} =
    A.object $
      [ "packageMeta"          .= pkgMeta
      , "version"              .= showVersion pkgVersion
      , "versionTag"           .= pkgVersionTag
      , "modules"              .= pkgModules
      , "bookmarks"            .= map (fmap (first P.runModuleName)) pkgBookmarks
      , "resolvedDependencies" .= assocListToJSON runPackageName
                                                  (T.pack . showVersion)
                                                  pkgResolvedDependencies
      , "github"               .= pkgGithub
      , "uploader"             .= pkgUploader
      , "compilerVersion"      .= showVersion P.version
      ] ++
      fmap (\t -> "tagTime" .= formatTime t) (maybeToList pkgTagTime)

instance A.ToJSON NotYetKnown where
  toJSON _ = A.Null

instance A.ToJSON Module where
  toJSON Module{..} =
    A.object [ "name"         .= P.runModuleName modName
             , "comments"     .= modComments
             , "declarations" .= modDeclarations
             , "reExports"    .= map toObj modReExports
             ]
    where
    toObj (mn, decls) = A.object [ "moduleName" .= mn
                                 , "declarations" .= decls
                                 ]

instance A.ToJSON Declaration where
  toJSON Declaration{..} =
    A.object [ "title"      .= declTitle
             , "comments"   .= declComments
             , "sourceSpan" .= declSourceSpan
             , "children"   .= declChildren
             , "info"       .= declInfo
             ]

instance A.ToJSON ChildDeclaration where
  toJSON ChildDeclaration{..} =
    A.object [ "title"      .= cdeclTitle
             , "comments"   .= cdeclComments
             , "sourceSpan" .= cdeclSourceSpan
             , "info"       .= cdeclInfo
             ]

instance A.ToJSON DeclarationInfo where
  toJSON info = A.object $ "declType" .= declInfoToString info : props
    where
    props = case info of
      ValueDeclaration ty -> ["type" .= ty]
      DataDeclaration ty args -> ["dataDeclType" .= ty, "typeArguments" .= args]
      ExternDataDeclaration kind -> ["kind" .= kind]
      TypeSynonymDeclaration args ty -> ["arguments" .= args, "type" .= ty]
      TypeClassDeclaration args super fundeps -> ["arguments" .= args, "superclasses" .= super, "fundeps" .= fundeps]
      AliasDeclaration fixity alias -> ["fixity" .= fixity, "alias" .= alias]
      ExternKindDeclaration -> []

instance A.ToJSON ChildDeclarationInfo where
  toJSON info = A.object $ "declType" .= childDeclInfoToString info : props
    where
    props = case info of
      ChildInstance deps ty     -> ["dependencies" .= deps, "type" .= ty]
      ChildDataConstructor args -> ["arguments" .= args]
      ChildTypeClassMember ty   -> ["type" .= ty]

instance A.ToJSON GithubUser where
  toJSON = A.toJSON . runGithubUser

instance A.ToJSON GithubRepo where
  toJSON = A.toJSON . runGithubRepo

-- | Given a function for turning association list keys into JSON object keys,
-- and a function for turning association list values to JSON string values,
-- turns an association list into a JSON object.
--
-- For example:
-- @assocListToJSON T.pack T.pack [("a", "b")]@ will give @{"a": "b"}@.
assocListToJSON :: (a -> Text) -> (b -> Text) -> [(a, b)] -> A.Value
assocListToJSON f g xs = A.object (map (uncurry (.=) . (f *** g)) xs)

instance A.ToJSON a => A.ToJSON (InPackage a) where
  toJSON x =
    case x of
      Local y      -> withPackage (Nothing :: Maybe ()) y
      FromDep pn y -> withPackage (Just pn) y
    where
    withPackage :: (A.ToJSON p, A.ToJSON x) => p -> x -> A.Value
    withPackage p y =
      A.object [ "package" .= p
               , "item"    .= y
               ]
