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
import Data.Aeson ((.=))
import qualified Data.Aeson as A
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

--------------------
-- Types

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

data RenderedModule = RenderedModule
  { rmName         :: String
  , rmComments     :: Maybe String
  , rmDeclarations :: [RenderedDeclaration]
  }
  deriving (Show, Eq, Ord)

data RenderedDeclaration = RenderedDeclaration
  { rdTitle      :: String
  , rdComments   :: Maybe String
  , rdCode       :: RenderedCode
  , rdSourceSpan :: Maybe P.SourceSpan
  , rdChildren   :: [RenderedChildDeclaration]
  }
  deriving (Show, Eq, Ord)

data RenderedChildDeclaration = RenderedChildDeclaration
  { rcdTitle      :: String
  , rcdComments   :: Maybe String
  , rcdCode       :: RenderedCode
  , rcdSourceSpan :: Maybe P.SourceSpan
  , rcdType       :: RenderedChildDeclarationType
  }
  deriving (Show, Eq, Ord)

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

newtype GithubUser
  = GithubUser { runGithubUser :: String }
  deriving (Show, Eq, Ord)

newtype GithubRepo
  = GithubRepo { runGithubRepo :: String }
  deriving (Show, Eq, Ord)

data UploadedPackageError
  = ErrorInPackageMeta BowerError
  | InvalidVersion
  | InvalidDeclarationType
  | InvalidRenderedCode String
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

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just vers
    _            -> Nothing

asRenderedModule :: Parse UploadedPackageError RenderedModule
asRenderedModule =
  RenderedModule <$> key "name" asString
                 <*> key "comments" (perhaps asString)
                 <*> key "declarations" (eachInArray asDeclaration)

asDeclaration :: Parse UploadedPackageError RenderedDeclaration
asDeclaration =
  RenderedDeclaration <$> key "title" asString
                      <*> key "comments" (perhaps asString)
                      <*> key "code" asRenderedCode .! InvalidRenderedCode
                      <*> key "sourceSpan" (perhaps asSourceSpan)
                      <*> key "children" (eachInArray asRenderedChildDeclaration)

asRenderedChildDeclaration :: Parse UploadedPackageError RenderedChildDeclaration
asRenderedChildDeclaration =
  RenderedChildDeclaration <$> key "title" asString
                           <*> key "comments" (perhaps asString)
                           <*> key "code" asRenderedCode .! InvalidRenderedCode
                           <*> key "sourceSpan" (perhaps asSourceSpan)
                           <*> key "type" asRenderedChildDeclarationType .!! InvalidDeclarationType
  where
  p .!! err = p .! const err

asRenderedChildDeclarationType :: Parse UploadedPackageError RenderedChildDeclarationType
asRenderedChildDeclarationType =
  withString (maybe (Left InvalidDeclarationType) Right .
                flip lookup childDeclarationTypes)

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

asGithub :: Parse e (GithubUser, GithubRepo)
asGithub = (,) <$> nth 0 (GithubUser <$> asString)
               <*> nth 1 (GithubRepo <$> asString)

asSourceSpan :: Parse e P.SourceSpan
asSourceSpan = P.SourceSpan <$> key "name" asString
                            <*> key "start" asSourcePos
                            <*> key "end" asSourcePos

---------------------
-- ToJSON instances

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

instance A.ToJSON RenderedModule where
  toJSON RenderedModule{..} =
    A.object [ "name"         .= rmName
             , "comments"     .= rmComments
             , "declarations" .= rmDeclarations
             ]

instance A.ToJSON RenderedDeclaration where
  toJSON RenderedDeclaration{..} =
    A.object [ "title"      .= rdTitle
             , "comments"   .= rdComments
             , "code"       .= rdCode
             , "sourceSpan" .= rdSourceSpan
             , "children"   .= rdChildren
             ]

instance A.ToJSON RenderedChildDeclaration where
  toJSON RenderedChildDeclaration{..} =
    A.object [ "title"      .= rcdTitle
             , "comments"   .= rcdComments
             , "code"       .= rcdCode
             , "sourceSpan" .= rcdSourceSpan
             , "type"       .= rcdType
             ]

instance A.ToJSON RenderedChildDeclarationType where
  toJSON = A.toJSON . childDeclTypeToString

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
