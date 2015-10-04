{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Docs.Types
  ( module Language.PureScript.Docs.Types
  , module ReExports
  )
  where

import Control.Arrow (first, (***))
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<$), (<*>), pure)
#endif
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Data.Version
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Aeson.BetterErrors
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T

import Web.Bower.PackageMeta hiding (Version, displayError)

import qualified Language.PureScript as P

import Language.PureScript.Docs.RenderedCode as ReExports
  (RenderedCode, asRenderedCode,
   ContainingModule(..), asContainingModule,
   RenderedCodeElement(..), asRenderedCodeElement)

--------------------
-- Types

data Package a = Package
  { pkgMeta                 :: PackageMeta
  , pkgVersion              :: Version
  , pkgVersionTag           :: String
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
          pkgModules
          pkgBookmarks
          pkgResolvedDependencies
          pkgGithub
          verifiedUser
          pkgCompilerVersion

packageName :: Package a -> PackageName
packageName = bowerName . pkgMeta

data Module = Module
  { modName         :: String
  , modComments     :: Maybe String
  , modDeclarations :: [Declaration]
  }
  deriving (Show, Eq, Ord)

data Declaration = Declaration
  { declTitle      :: String
  , declComments   :: Maybe String
  , declSourceSpan :: Maybe P.SourceSpan
  , declChildren   :: [ChildDeclaration]
  , declFixity     :: Maybe P.Fixity
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
  | DataDeclaration P.DataDeclType [(String, Maybe P.Kind)]

  -- |
  -- A data type foreign import, with its kind.
  --
  | ExternDataDeclaration P.Kind

  -- |
  -- A type synonym, with its type arguments and its type.
  --
  | TypeSynonymDeclaration [(String, Maybe P.Kind)] P.Type

  -- |
  -- A type class, with its type arguments and its superclasses. Instances and
  -- members are represented as child declarations.
  --
  | TypeClassDeclaration [(String, Maybe P.Kind)] [P.Constraint]
  deriving (Show, Eq, Ord)

declInfoToString :: DeclarationInfo -> String
declInfoToString (ValueDeclaration _) = "value"
declInfoToString (DataDeclaration _ _) = "data"
declInfoToString (ExternDataDeclaration _) = "externData"
declInfoToString (TypeSynonymDeclaration _ _) = "typeSynonym"
declInfoToString (TypeClassDeclaration _ _) = "typeClass"

data ChildDeclaration = ChildDeclaration
  { cdeclTitle      :: String
  , cdeclComments   :: Maybe String
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

childDeclInfoToString :: ChildDeclarationInfo -> String
childDeclInfoToString (ChildInstance _ _)      = "instance"
childDeclInfoToString (ChildDataConstructor _) = "dataConstructor"
childDeclInfoToString (ChildTypeClassMember _) = "typeClassMember"

newtype GithubUser
  = GithubUser { runGithubUser :: String }
  deriving (Show, Eq, Ord)

newtype GithubRepo
  = GithubRepo { runGithubRepo :: String }
  deriving (Show, Eq, Ord)

data PackageError
  = CompilerTooOld Version Version
      -- ^ Minimum allowable version for generating data with the current
      -- parser, and actual version used.
  | ErrorInPackageMeta BowerError
  | InvalidVersion
  | InvalidDeclarationType String
  | InvalidChildDeclarationType String
  | InvalidFixity
  | InvalidKind String
  | InvalidDataDeclType String
  deriving (Show, Eq, Ord)

type Bookmark = InPackage (P.ModuleName, String)

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
          <*> key "versionTag" asString
          <*> key "modules" (eachInArray asModule)
          <*> key "bookmarks" asBookmarks .! ErrorInPackageMeta
          <*> key "resolvedDependencies" asResolvedDependencies
          <*> key "github" asGithub
          <*> key "uploader" uploader
          <*> pure compilerVersion

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
    "Invalid declaration type: \"" <> T.pack str <> "\""
  InvalidChildDeclarationType str ->
    "Invalid child declaration type: \"" <> T.pack str <> "\""
  InvalidFixity ->
    "Invalid fixity"
  InvalidKind str ->
    "Invalid kind: \"" <> T.pack str <> "\""
  InvalidDataDeclType str ->
    "Invalid data declaration type: \"" <> T.pack str <> "\""
  where
  (<>) = T.append

instance A.FromJSON a => A.FromJSON (Package a) where
  parseJSON = toAesonParser displayPackageError
                            (asPackage (Version [0,0,0,0] []) fromAesonParser)

asGithubUser :: Parse e GithubUser
asGithubUser = GithubUser <$> asString

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
  Module <$> key "name" asString
         <*> key "comments" (perhaps asString)
         <*> key "declarations" (eachInArray asDeclaration)

asDeclaration :: Parse PackageError Declaration
asDeclaration =
  Declaration <$> key "title" asString
              <*> key "comments" (perhaps asString)
              <*> key "sourceSpan" (perhaps asSourceSpan)
              <*> key "children" (eachInArray asChildDeclaration)
              <*> key "fixity" (perhaps asFixity)
              <*> key "info" asDeclarationInfo

asFixity :: Parse PackageError P.Fixity
asFixity = P.Fixity <$> key "associativity" asAssociativity
                    <*> key "precedence" asIntegral

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
  ty <- key "declType" asString
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
    other ->
      throwCustomError (InvalidDeclarationType other)

asTypeArguments :: Parse PackageError [(String, Maybe P.Kind)]
asTypeArguments = eachInArray asTypeArgument
  where
  asTypeArgument = (,) <$> nth 0 asString <*> nth 1 (perhaps asKind)

asKind :: Parse e P.Kind
asKind = fromAesonParser

asType :: Parse e P.Type
asType = fromAesonParser

asDataDeclType :: Parse PackageError P.DataDeclType
asDataDeclType =
  withString $ \s -> case s of
    "data"    -> Right P.Data
    "newtype" -> Right P.Newtype
    other     -> Left (InvalidDataDeclType other)

asChildDeclaration :: Parse PackageError ChildDeclaration
asChildDeclaration =
  ChildDeclaration <$> key "title" asString
                           <*> key "comments" (perhaps asString)
                           <*> key "sourceSpan" (perhaps asSourceSpan)
                           <*> key "info" asChildDeclarationInfo

asChildDeclarationInfo :: Parse PackageError ChildDeclarationInfo
asChildDeclarationInfo = do
  ty <- key "declType" asString
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
asConstraint = (,) <$> nth 0 asQualifiedProperName
                   <*> nth 1 (eachInArray asType)

asQualifiedProperName :: Parse e (P.Qualified P.ProperName)
asQualifiedProperName = fromAesonParser

asBookmarks :: Parse BowerError [Bookmark]
asBookmarks = eachInArray asBookmark

asBookmark :: Parse BowerError Bookmark
asBookmark =
  build <$> key "package" (perhaps (withString parsePackageName))
        <*> key "item" ((,) <$> nth 0 (P.moduleNameFromString <$> asString)
                            <*> nth 1 asString)
  where
  build Nothing = Local
  build (Just pn) = FromDep pn

asResolvedDependencies :: Parse PackageError [(PackageName, Version)]
asResolvedDependencies =
  eachInObjectWithKey (mapLeft ErrorInPackageMeta . parsePackageName . T.unpack) asVersion
  where
  mapLeft f (Left x) = Left (f x)
  mapLeft _ (Right x) = Right x

asGithub :: Parse e (GithubUser, GithubRepo)
asGithub = (,) <$> nth 0 (GithubUser <$> asString)
               <*> nth 1 (GithubRepo <$> asString)

asSourceSpan :: Parse e P.SourceSpan
asSourceSpan = P.SourceSpan <$> key "name" asString
                            <*> key "start" asSourcePos
                            <*> key "end" asSourcePos

---------------------
-- ToJSON instances

instance A.ToJSON a => A.ToJSON (Package a) where
  toJSON Package{..} =
    A.object
      [ "packageMeta"          .= pkgMeta
      , "version"              .= showVersion pkgVersion
      , "versionTag"           .= pkgVersionTag
      , "modules"              .= pkgModules
      , "bookmarks"            .= map (fmap (first P.runModuleName)) pkgBookmarks
      , "resolvedDependencies" .= assocListToJSON (T.pack . runPackageName)
                                                  (T.pack . showVersion)
                                                  pkgResolvedDependencies
      , "github"               .= pkgGithub
      , "uploader"             .= pkgUploader
      , "compilerVersion"      .= showVersion P.version
      ]

instance A.ToJSON NotYetKnown where
  toJSON _ = A.Null

instance A.ToJSON Module where
  toJSON Module{..} =
    A.object [ "name"         .= modName
             , "comments"     .= modComments
             , "declarations" .= modDeclarations
             ]

instance A.ToJSON Declaration where
  toJSON Declaration{..} =
    A.object [ "title"      .= declTitle
             , "comments"   .= declComments
             , "sourceSpan" .= declSourceSpan
             , "children"   .= declChildren
             , "fixity"     .= declFixity
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
      TypeClassDeclaration args super -> ["arguments" .= args, "superclasses" .= super]

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
