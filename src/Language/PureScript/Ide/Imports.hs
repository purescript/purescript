-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Imports
-- Description : Provides functionality to manage imports
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Provides functionality to manage imports
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}

module Language.PureScript.Ide.Imports
       ( addImplicitImport
       , addImportForIdentifier
       , answerRequest
         -- for tests
       , addImplicitImport'
       , addExplicitImport'
       , sliceImportSection
       , prettyPrintImport'
       , Import(Import)
       )
       where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           "monad-logger" Control.Monad.Logger
import qualified Data.List                          as List
import           Data.Maybe                         (mapMaybe)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import qualified Language.PureScript                as P
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types

data Import = Import P.ModuleName P.ImportDeclarationType  (Maybe P.ModuleName)
              deriving (Eq, Show)

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                        FilePath -> m ([Text], [Import], [Text])
parseImportsFromFile fp = do
  file <- liftIO (TIO.readFile fp)
  pure (sliceImportSection (T.lines file))

sliceImportSection :: [Text] -> ([Text], [Import], [Text])
sliceImportSection ls =
  let
    preImportSection = takeWhile (not . hasImportPrefix) ls
    importSection =
      takeWhile continuesImport $
        dropWhile (not . hasImportPrefix) ls
    postImportSection =
      dropWhile continuesImport $
        dropWhile (not . hasImportPrefix) ls
    hasImportPrefix = T.isPrefixOf "import"
    continuesImport x = hasImportPrefix x || T.isPrefixOf " " x || x == ""
  in (preImportSection, parseImports importSection, postImportSection)

parseImports :: [Text] -> [Import]
parseImports ts =
  let
    concatMultilineImports = foldl step [] ts
    step :: [Text] -> Text -> [Text]
    step acc t = if T.isPrefixOf " " t
                 then init acc ++ [last acc <> t]
                 else acc ++ [t]
  in
    mapMaybe parseImport concatMultilineImports

parseImport :: Text -> Maybe Import
parseImport t =
  let
    parseResult = do
      tokens <- P.lex "" (T.unpack t)
      P.runTokenParser "" P.parseImportDeclaration' tokens
  in
    case parseResult of
      Right (mn, idt, mmn, _) -> Just (Import mn idt mmn)
      Left _ -> Nothing


-- | Adds an implicit import like @import Prelude@ to a Sourcefile.
addImplicitImport :: (MonadIO m, MonadError PscIdeError m)
                     => FilePath     -- ^ The Sourcefile read from
                     -> P.ModuleName -- ^ The module to import
                     -> m [Text]
addImplicitImport fp mn = do
  (pre, imports, post) <- parseImportsFromFile fp
  let newImportSection = addImplicitImport' imports mn
  pure $ pre
    ++ newImportSection
    ++ post

addImplicitImport' :: [Import] -> P.ModuleName -> [Text]
addImplicitImport' imports mn =
  List.sort (map prettyPrintImport' (imports ++ [Import mn P.Implicit Nothing])) ++ [""]

addExplicitImport :: (MonadIO m, MonadError PscIdeError m, MonadLogger m) =>
                     FilePath -> Text -> P.ModuleName -> m [Text]
addExplicitImport fp identifier moduleName = do
  (pre, imports, post) <- parseImportsFromFile fp
  logDebugN ("Identifier: " <> identifier <> "ModuleName: " <> T.pack (P.runModuleName moduleName))
  let newImportSection = addExplicitImport' (P.Ident (T.unpack identifier)) moduleName imports
  pure (pre ++ newImportSection ++ post)

addExplicitImport' :: P.Ident -> P.ModuleName -> [Import] -> [Text]
addExplicitImport' identifier moduleName imports =
  let
    matches (Import mn (P.Explicit _) Nothing) = mn == moduleName
    matches _ = False

    newImports = case List.findIndex matches imports of
      -- The module wasn't imported yet
      Nothing ->
        imports ++ [Import moduleName (P.Explicit [P.ValueRef identifier]) Nothing]
      Just ix ->
        let (x, Import mn (P.Explicit refs) Nothing : ys) = List.splitAt ix imports
        in x  ++ [Import mn (P.Explicit (P.ValueRef identifier : refs)) Nothing] ++ ys

  in List.sort (map prettyPrintImport' newImports) ++ [""]


-- | Looks up the given identifier in the currently loaded modules.
--
-- * Throws an error if the identifier cannot be found.
--
-- * If exactly one match is found, adds an explicit import to the importsection
--
-- * If more than one possible imports are found, reports the possibilities as a
-- list of completions.
addImportForIdentifier :: (PscIde m, MonadError PscIdeError m, MonadLogger m)
                          => FilePath -- ^ The Sourcefile to read from
                          -> Text     -- ^ The identifier to import
                          -> [Filter] -- ^ Filters to apply before searching for
                                      -- the identifier
                          -> m (Either [Completion] [Text])
addImportForIdentifier fp ident filters = do
  modules <- getAllModulesWithReexports
  case getExactMatches ident filters modules of
    [] ->
      throwError (NotFound "Couldn't find the given identifier.\
                           \Have you loaded the corresponding module?")

    -- Only one match was found for the given identifier, so we can insert it
    -- right away
    [Completion (m, i, _)] ->
      Right <$> addExplicitImport fp i (P.moduleNameFromString (T.unpack m))

    -- Multiple matches where found so we need to ask the user to clarify which
    -- module he meant
    xs ->
      pure $ Left xs

prettyPrintImport' :: Import -> Text
prettyPrintImport' (Import mn idt qual) =
  T.pack $ "import " ++ P.prettyPrintImport mn idt qual

answerRequest :: (MonadIO m) => Maybe FilePath -> [Text] -> m Success
answerRequest outfp rs  =
  case outfp of
    Nothing -> pure $ MultilineTextResult rs
    Just outfp' -> do
      liftIO $ TIO.writeFile outfp' (T.unlines rs)
      pure $ TextResult $ "Written to " <> T.pack outfp'
