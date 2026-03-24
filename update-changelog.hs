#!/usr/bin/env stack
{- stack
  --resolver lts-20.9 script
  --package bytestring
  --package filepath
  --package text
  --package github-rest
  --package directory
  --package simple-cmd
  --package time
  --package bifunctors
  --package attoparsec
  --package aeson
  --package protolude
-}
{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , FlexibleContexts
  , LambdaCase
  , NoImplicitPrelude
  , OverloadedStrings
  , PackageImports
  , RecordWildCards
  , TupleSections
  , ViewPatterns
 #-} -- Hlint requires this leading space

-- |
-- This script updates CHANGELOG.md with the contents of CHANGELOG.d, and
-- empties CHANGELOG.d. It takes care of:
--
-- * Sorting entries by the order in which their PRs were merged
-- * Appending (#1234 by @author) to the first line of each fragment,
--   optionally adding multiple PR numbers and/or authors as applicable
-- * Grouping entries by type and adding non-empty group headings to the
--   changelog
-- * Syncing any affected files to the Git index, preparing for you to make
--   your release commit
--
-- Be sure to run this *after* updating the version number in
-- npm-package/package.json, as that's where this script gets the new section
-- header from.
--

module Main (main) where

import Protolude hiding (intercalate, readFile, writeFile)
import qualified Protolude

import           Control.Monad.Fail (fail)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import           Data.Attoparsec.ByteString (maybeResult, parse)
import "bifunctors"
                 Data.Bifunctor.Flip (Flip(..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NEL
import           Data.String (String)
import qualified Data.String as String
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Data.Time.LocalTime (zonedTimeToUTC)
import           GitHub.REST (GHEndpoint(..), GitHubSettings(..), KeyValue(..), MonadGitHubREST, StdMethod(..), queryGitHub, runGitHubT)
import qualified SimpleCmd.Git as IOGit
import           System.Directory (setCurrentDirectory)
import           System.FilePath (normalise, takeFileName, (</>))

main = runGitHubT gitHubSettings $ do
  git "rev-parse" ["--show-toplevel"] >>= liftIO . setCurrentDirectory
  entries <- String.lines <$> git "ls-tree" ["--name-only", "HEAD", "CHANGELOG.d/"]

  breaks   <- processEntriesStartingWith "break" entries
  features <- processEntriesStartingWith "feat" entries
  fixes    <- processEntriesStartingWith "fix" entries
  internal <- processEntriesStartingWith "int" entries
  misc     <- processEntriesStartingWith "misc" entries

  let entryFiles = ceFile <$> breaks <> features <> fixes <> internal <> misc
  unless (null entryFiles) $ do

    changes <- git "status" ("-s" : "--" : "CHANGELOG.md" : entryFiles)
    unless (null changes) . liftIO . die $
      "You have uncommitted changes to changelog files. " <>
      "Please commit, stash, or revert them before running this script."

    version <- getVersion
    (changelogPreamble, changelogRest) <- T.breakOn "\n## " <$> readFile "CHANGELOG.md"
    writeFile "CHANGELOG.md" $
         changelogPreamble
      <> "\n## " <> version <> "\n"
      <> conditionalSection "Breaking changes" breaks
      <> conditionalSection "New features" features
      <> conditionalSection "Bugfixes" fixes
      <> conditionalSection "Other improvements" misc
      <> conditionalSection "Internal" internal
      <> changelogRest

    git_ "add" ["CHANGELOG.md"]
    git_ "rm" $ "-q" : entryFiles

gitHubSettings :: GitHubSettings
gitHubSettings = GitHubSettings Nothing "purescript/purescript update-changelog.hs" "v3"

processEntriesStartingWith :: (MonadFail m, MonadGitHubREST m, MonadIO m) => String -> [String] -> m [ChangelogEntry]
processEntriesStartingWith prefix
  = fmap (sortOn ceDate)
  . traverse updateEntry
  . filter ((prefix `isPrefixOf`) . map toLower . takeFileName)

updateEntry :: (MonadFail m, MonadGitHubREST m, MonadIO m) => String -> m ChangelogEntry
updateEntry file = do
  (header, body) <- T.breakOn "\n" . T.strip <$> (readFile . normalise) file

  allCommits <-
        fmap (NEL.fromList . sortOn glcTime)
    .   traverse (\(T.breakOn " " -> (h, T.breakOn " " . T.tail -> (c, s))) ->
          GitLogCommit (T.tail s) h . zonedTimeToUTC <$> iso8601ParseM (toS c))
    =<< gitLines "log" ["-m", "--follow", "--format=%H %cI %s", file]

  prCommits <-
      filterM isInterestingCommit
    . mapMaybe (traverse parsePRNumber)
    $ NEL.toList allCommits

  let prNumbers = map (snd . glcData) prCommits

  prAuthors <- ordNub <$> traverse lookupPRAuthor prNumbers

  let headerSuffix = if null prNumbers then "" else
           " ("
        <> commaSeparate (map (("#" <>) . show) prNumbers)
        <> " by "
        <> commaSeparate (map ("@" <>) prAuthors)
        <> ")"

  pure $ ChangelogEntry file (header <> headerSuffix <> body <> "\n") (glcTime $ NEL.head allCommits)

parsePRNumber :: Text -> Maybe (CommitType, Int)
parsePRNumber = liftA2 (<|>)
  (fmap (MergeCommit, ) . readMaybe . (toS :: T.Text -> String) . fst . T.breakOn " " <=< T.stripPrefix "Merge pull request #")
  (fmap (SquashCommit, ) . readMaybe . (toS :: T.Text -> String) <=< T.stripSuffix ")" . snd . T.breakOnEnd "(#")

-- |
-- This function helps us exclude PRs that are just fixups of changelog
-- wording. An interesting commit is one that has either edited a file that
-- isn't part of the changelog, or is a merge commit.
--
isInterestingCommit :: MonadIO m => GitLogCommit (CommitType, Int) -> m Bool
isInterestingCommit GitLogCommit{..} = case fst glcData of
  MergeCommit -> pure True
  SquashCommit ->
    not . all (\path -> "CHANGELOG.md" == path || "CHANGELOG.d/" `T.isPrefixOf` path)
      <$> gitLines "show" ["--format=", "--name-only", toS glcHash]

lookupPRAuthor :: (MonadFail m, MonadGitHubREST m) => Int -> m Text
lookupPRAuthor prNum =
  queryGitHub GHEndpoint{ method = GET
                        , endpoint = "/repos/purescript/purescript/pulls/:pr"
                        , endpointVals = ["pr" := prNum]
                        , ghData = []
                        }
    >>= \case
      JSON.Object (KM.lookup "user" -> Just (JSON.Object (KM.lookup "login" -> Just (JSON.String name)))) -> pure name
      _ -> fail "error accessing GitHub API"

commaSeparate :: [Text] -> Text
commaSeparate = \case
  [] -> ""
  [a] -> a
  [a, b] -> a <> " and " <> b
  more | Just (init, last) <- unsnoc more -> T.intercalate ", " init <> ", and " <> last

getVersion :: (MonadFail m, MonadIO m) => m Text
getVersion =
  (liftIO . BS.readFile) ("npm-package" </> "package.json") >>= \case
    (maybeResult . parse JSON.json -> Just (JSON.Object (KM.lookup "version" -> Just (JSON.String v)))) -> pure v
    _ -> fail "could not read version from npm-package/package.json"

conditionalSection :: Text -> [ChangelogEntry] -> Text
conditionalSection header = \case
  [] -> ""
  entries ->
    "\n" <> header <> ":\n\n" <> T.intercalate "\n" (map ceContent entries)

git :: MonadIO m => String -> [String] -> m String
git cmd = liftIO . IOGit.git cmd

git_ :: MonadIO m => String -> [String] -> m ()
git_ cmd = liftIO . IOGit.git_ cmd

gitLines :: MonadIO m => String -> [String] -> m [Text]
gitLines cmd args = lines . toS <$> git cmd args

readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . Protolude.readFile

writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile path = liftIO . Protolude.writeFile path

data ChangelogEntry = ChangelogEntry
  { ceFile :: String
  , ceContent :: Text
  , ceDate :: UTCTime
  }

data GitLogCommit a = GitLogCommit
  { glcData :: a
  , glcHash :: Text
  , glcTime :: UTCTime
  }
  deriving (Functor, Foldable, Traversable)

data CommitType = MergeCommit | SquashCommit
