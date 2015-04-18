{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.Docs.Types
  ( module Language.PureScript.Docs.Types
  , module ReExports
  )
  where

import Control.Arrow (first, (***))
import Control.Applicative ((<$>), (<*>))
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Version
import qualified Data.Map as M
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.BetterErrors
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Text (Text)
import qualified Data.Text as T

import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript as P

import Language.PureScript.Docs.RenderedCode as ReExports
  (RenderedCode, asRenderedCode,
   ContainingModule(..), asContainingModule,
   RenderedCodeElement(..), asRenderedCodeElement)

data UploadedPackage = UploadedPackage
  { pkgMeta                 :: PackageMeta
  , pkgVersion              :: Version
  , pkgModules              :: [RenderedModule]
  , pkgBookmarks            :: [Bookmark]
  , pkgResolvedDependencies :: [(PackageName, Version)]
  , pkgGithub               :: (GithubUser, GithubRepo)
  , pkgUploader             :: Maybe GithubUser
  }
  deriving (Show, Eq, Ord)

instance A.ToJSON UploadedPackage where
  toJSON UploadedPackage{..} =
    A.object $
      [ "packageMeta"          .= pkgMeta
      , "version"              .= showVersion pkgVersion
      , "modules"              .= pkgModules
      , "bookmarks"            .= map (fmap (first P.runModuleName)) pkgBookmarks
      , "resolvedDependencies" .= assocListToJSON (T.pack . runPackageName)
                                                  (T.pack . showVersion)
                                                  pkgResolvedDependencies
      , "github"               .= pkgGithub
      ] ++ case pkgUploader of
             Just u  -> [ "uploader" .= u ]
             Nothing -> []

assocListToJSON :: (a -> Text) -> (b -> Text) -> [(a, b)] -> A.Value
assocListToJSON f g xs = A.object (map (uncurry (.=) . (f *** g)) xs)

data UploadedPackageError
  = ErrorInPackageMeta BowerError
  | InvalidVersion
  | InvalidDeclarationType
  | InvalidRenderedCode String
  deriving (Show, Eq, Ord)

asUploadedPackage :: Parse UploadedPackageError UploadedPackage
asUploadedPackage =
  UploadedPackage <$> key "packageMeta" asPackageMeta .! ErrorInPackageMeta
                  <*> key "version" asVersion
                  <*> key "modules" (eachInArray asRenderedModule)
                  <*> key "bookmarks" asBookmarks .! ErrorInPackageMeta
                  <*> key "resolvedDependencies" asResolvedDependencies
                  <*> key "github" asGithub
                  <*> keyMay "uploader" (GithubUser <$> asString)

asVersion :: Parse UploadedPackageError Version
asVersion = withString (maybe (Left InvalidVersion) Right . parseVersion')

asRenderedModule :: Parse UploadedPackageError RenderedModule
asRenderedModule =
  RenderedModule <$> key "name" asString
                 <*> key "comments" (perhaps asString)
                 <*> key "declarations" (eachInArray asDeclaration)

asDepsModules :: Parse UploadedPackageError (M.Map P.ModuleName PackageName)
asDepsModules =
  M.fromList <$> eachInObjectWithKey (Right . P.moduleNameFromString . T.unpack)
                                     (withString parsePackageName) .! ErrorInPackageMeta

asGithub :: Parse e (GithubUser, GithubRepo)
asGithub = (,) <$> nth 0 (GithubUser <$> asString)
               <*> nth 1 (GithubRepo <$> asString)

asSourceSpan :: Parse e P.SourceSpan
asSourceSpan = P.SourceSpan <$> key "name" asString
                            <*> key "start" asSourcePos
                            <*> key "end" asSourcePos

asSourcePos :: Parse e P.SourcePos
asSourcePos = P.SourcePos <$> nth 0 asIntegral
                          <*> nth 1 asIntegral

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

asResolvedDependencies :: Parse UploadedPackageError [(PackageName, Version)]
asResolvedDependencies =
  eachInObjectWithKey (mapLeft ErrorInPackageMeta . parsePackageName . T.unpack) asVersion
  where
  mapLeft f (Left x) = Left (f x)
  mapLeft _ (Right x) = Right x

newtype GithubUser
  = GithubUser { runGithubUser :: String }
  deriving (Show, Eq, Ord)

instance A.ToJSON GithubUser where
  toJSON = A.toJSON . runGithubUser

newtype GithubRepo
  = GithubRepo { runGithubRepo :: String }
  deriving (Show, Eq, Ord)

instance A.ToJSON GithubRepo where
  toJSON = A.toJSON . runGithubRepo

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just vers
    _            -> Nothing

data RenderedModule = RenderedModule
  { rmName         :: String
  , rmComments     :: Maybe String
  , rmDeclarations :: [RenderedDeclaration]
  }
  deriving (Show, Eq, Ord)

instance A.ToJSON RenderedModule where
  toJSON RenderedModule{..} =
    A.object [ "name"         .= rmName
             , "comments"     .= rmComments
             , "declarations" .= rmDeclarations
             ]

data RenderedDeclaration = RenderedDeclaration
  { rdTitle      :: String
  , rdComments   :: Maybe String
  , rdCode       :: RenderedCode
  , rdSourceSpan :: Maybe P.SourceSpan
  , rdChildren   :: [RenderedChildDeclaration]
  }
  deriving (Show, Eq, Ord)

instance A.ToJSON RenderedDeclaration where
  toJSON RenderedDeclaration{..} =
    A.object [ "title"      .= rdTitle
             , "comments"   .= rdComments
             , "code"       .= rdCode
             , "sourceSpan" .= rdSourceSpan
             , "children"   .= rdChildren
             ]

asDeclaration :: Parse UploadedPackageError RenderedDeclaration
asDeclaration =
  RenderedDeclaration <$> key "title" asString
                      <*> key "comments" (perhaps asString)
                      <*> key "code" asRenderedCode .! InvalidRenderedCode
                      <*> key "sourceSpan" (perhaps asSourceSpan)
                      <*> key "children" (eachInArray asRenderedChildDeclaration)

data RenderedChildDeclaration = RenderedChildDeclaration
  { rcdTitle      :: String
  , rcdComments   :: Maybe String
  , rcdCode       :: RenderedCode
  , rcdSourceSpan :: Maybe P.SourceSpan
  , rcdType       :: RenderedChildDeclarationType
  }
  deriving (Show, Eq, Ord)

instance A.ToJSON RenderedChildDeclaration where
  toJSON RenderedChildDeclaration{..} =
    A.object [ "title"      .= rcdTitle
             , "comments"   .= rcdComments
             , "code"       .= rcdCode
             , "sourceSpan" .= rcdSourceSpan
             , "type"       .= rcdType
             ]

asRenderedChildDeclaration :: Parse UploadedPackageError RenderedChildDeclaration
asRenderedChildDeclaration =
  RenderedChildDeclaration <$> key "title" asString
                           <*> key "comments" (perhaps asString)
                           <*> key "code" asRenderedCode .! InvalidRenderedCode
                           <*> key "sourceSpan" (perhaps asSourceSpan)
                           <*> key "type" asRenderedChildDeclarationType .!! InvalidDeclarationType
  where
  p .!! err = p .! const err

data RenderedChildDeclarationType
  = ChildInstance
  | ChildDataConstructor
  | ChildTypeClassMember
  deriving (Show, Eq, Ord, Bounded, Enum)

childDeclTypeToString :: RenderedChildDeclarationType -> String
childDeclTypeToString = withHead toLower . drop 5 . show
  where
  withHead f (x:xs) = f x : xs
  withHead _ [] = []

childDeclarationTypes :: [(String, RenderedChildDeclarationType)]
childDeclarationTypes =
  map (\t -> (childDeclTypeToString t, t)) [minBound .. maxBound]

instance A.ToJSON RenderedChildDeclarationType where
  toJSON = A.toJSON . childDeclTypeToString

asRenderedChildDeclarationType :: Parse UploadedPackageError RenderedChildDeclarationType
asRenderedChildDeclarationType =
  withString (maybe (Left InvalidDeclarationType) Right .
                flip lookup childDeclarationTypes)

type Bookmark = InPackage (P.ModuleName, String)

data InPackage a
  = Local a
  | FromDep PackageName a
  deriving (Show, Eq, Ord)

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

takeLocal :: InPackage a -> Maybe a
takeLocal (Local a) = Just a
takeLocal _ = Nothing

takeLocals :: [InPackage a] -> [a]
takeLocals = mapMaybe takeLocal

ignorePackage :: InPackage a -> a
ignorePackage (Local x) = x
ignorePackage (FromDep _ x) = x

ignorePackages :: [InPackage a] -> [a]
ignorePackages = map ignorePackage

instance Functor InPackage where
  fmap f (Local x) = Local (f x)
  fmap f (FromDep pkgName x) = FromDep pkgName (f x)
