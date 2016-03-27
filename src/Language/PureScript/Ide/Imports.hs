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
       , parseImport
       , prettyPrintImportSection
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
import           Data.Bifunctor (first, second)
import           Data.Function (on)
import qualified Data.List                          as List
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as TIO
import qualified Language.PureScript                as P
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Externs ( unwrapPositionedRef
                                                 , unwrapPositioned)
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

data Import = Import P.ModuleName P.ImportDeclarationType  (Maybe P.ModuleName)
              deriving (Eq, Show)

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                        FilePath -> m (P.ModuleName, [Text], [Import], [Text])
parseImportsFromFile fp = do
  file <- liftIO (TIO.readFile fp)
  case sliceImportSection (T.lines file) of
    Right res -> pure res
    Left err -> throwError (GeneralError err)

parseImportsWithModuleName :: [Text] -> Either String (P.ModuleName, [Import])
parseImportsWithModuleName ls = do
  (P.Module _ _ mn decls _) <- moduleParse ls
  pure (mn, concatMap mkImport (unwrapPositioned <$> decls))
  where
    mkImport (P.ImportDeclaration mn (P.Explicit refs) qual _) =
      [Import mn (P.Explicit (unwrapPositionedRef <$> refs)) qual]
    mkImport (P.ImportDeclaration mn it qual _) = [Import mn it qual]
    mkImport _ = []

sliceImportSection :: [Text] -> Either String (P.ModuleName, [Text], [Import], [Text])
sliceImportSection ts =
  case foldl step ModuleHeader (zip [0..] ts) of
    Res start end ->
      let
        (moduleHeader, (importSection, remainingFile)) =
          List.splitAt (succ (end - start)) `second` List.splitAt start ts
      in
        (\(mn, is) -> (mn, moduleHeader, is, remainingFile)) <$>
          parseImportsWithModuleName (moduleHeader <> importSection)
    _ -> Left "Failed to detect the import section"

data ImportStateMachine = ModuleHeader | ImportSection Int Int | Res Int Int

-- | We start in the
--
-- * ModuleHeader state.
--
-- We skip every line we encounter, that doesn't start with "import". Once we
-- find a line with "import" we store its linenumber as the start of the import
-- section and change into the
--
-- * ImportSection state
--
-- For any line that starts with import or whitespace(is thus indented) we
-- expand the end of the import section to that line and continue. If we
-- encounter a commented or empty line, we continue moving forward in the
-- ImportSection state but don't expand the import section end yet. This allows
-- us to exclude newlines or comments that directly follow the import section.
-- Once we encounter a line that is not a comment, newline, indentation or
-- import we switch into the
--
-- * Res state
--
-- , which just shortcuts to the end of the file and carries the detected import
-- section boundaries
step :: ImportStateMachine -> (Int, Text) -> ImportStateMachine
step ModuleHeader (ix, l) =
  if T.isPrefixOf "import" l then ImportSection ix ix else ModuleHeader
step (ImportSection start lastImportLine) (ix, l)
  | any (`T.isPrefixOf` l) ["import", " "] = ImportSection start ix
  | T.isPrefixOf "--" l || l == ""         = ImportSection start lastImportLine
  | otherwise                              = Res start lastImportLine
step (Res start end) _ = Res start end

moduleParse :: [Text] -> Either String P.Module
moduleParse t = first show $ do
  tokens <- (P.lex "" . T.unpack . T.unlines) t
  P.runTokenParser "<psc-ide>" P.parseModule tokens

-- | Adds an implicit import like @import Prelude@ to a Sourcefile.
addImplicitImport :: (MonadIO m, MonadError PscIdeError m)
                     => FilePath     -- ^ The Sourcefile read from
                     -> P.ModuleName -- ^ The module to import
                     -> m [Text]
addImplicitImport fp mn = do
  (_, pre, imports, post) <- parseImportsFromFile fp
  let newImportSection = addImplicitImport' imports mn
  pure $ pre ++ newImportSection ++ post

addImplicitImport' :: [Import] -> P.ModuleName -> [Text]
addImplicitImport' imports mn =
  prettyPrintImportSection (Import mn P.Implicit Nothing : imports)

-- | Adds an explicit import like @import Prelude (unit)@ to a Sourcefile. If an
-- explicit import already exists for the given module, it adds the identifier
-- to that imports list.
--
-- So @addExplicitImport "/File.purs" "bind" "Prelude"@ with an already existing
-- @import Prelude (bind)@ in the file File.purs returns @["import Prelude
-- (bind, unit)"]@
addExplicitImport :: (MonadIO m, MonadError PscIdeError m, MonadLogger m) =>
                     FilePath -> ExternDecl -> P.ModuleName -> m [Text]
addExplicitImport fp decl moduleName = do
  (mn, pre, imports, post) <- parseImportsFromFile fp
  let newImportSection =
        -- TODO: Open an issue when this PR is merged, we should optimise this
        -- so that this case does not write to disc
        if mn == moduleName
        then imports
        else addExplicitImport' decl moduleName imports
  pure (pre ++ prettyPrintImportSection newImportSection ++ post)

addExplicitImport' :: ExternDecl -> P.ModuleName -> [Import] -> [Import]
addExplicitImport' decl moduleName imports =
  let
    matches (Import mn (P.Explicit _) Nothing) = mn == moduleName
    matches _ = False
    freshImport = Import moduleName (P.Explicit [refFromDeclaration decl]) Nothing
  in
    updateAtFirstOrPrepend matches (insertDeclIntoImport decl) freshImport imports
  where
    refFromDeclaration (TypeClassDeclaration n) = P.TypeClassRef n
    refFromDeclaration (DataConstructor n tn _) = P.TypeRef tn (Just [P.ProperName (T.unpack n)])
    refFromDeclaration (TypeDeclaration n _) = P.TypeRef n (Just [])
    refFromDeclaration d = P.ValueRef (P.Ident (T.unpack (identifierFromExternDecl d)))

    -- | Adds a declaration to an import:
    -- TypeDeclaration "Maybe" + Data.Maybe (maybe) -> Data.Maybe(Maybe, maybe)
    insertDeclIntoImport :: ExternDecl -> Import -> Import
    insertDeclIntoImport decl' (Import mn (P.Explicit refs) Nothing) =
      Import mn (P.Explicit (insertDeclIntoRefs decl' refs)) Nothing
    insertDeclIntoImport _ is = is

    insertDeclIntoRefs :: ExternDecl -> [P.DeclarationRef] -> [P.DeclarationRef]
    insertDeclIntoRefs (DataConstructor dtor tn _) refs =
      let
        dtor' = P.ProperName (T.unpack dtor)
      in
        updateAtFirstOrPrepend (matchType tn) (insertDtor dtor') (P.TypeRef tn (Just [dtor'])) refs
    insertDeclIntoRefs dr refs = List.nubBy ((==) `on` P.prettyPrintRef) (refFromDeclaration dr : refs)

    insertDtor dtor (P.TypeRef tn' dtors) =
      case dtors of
        Just dtors' -> P.TypeRef tn' (Just (List.nub (dtor : dtors')))
        -- This means only the type was imported so far
        -- import Data.Maybe (Maybe) -> import Data.Maybe (Maybe(Just))
        Nothing -> P.TypeRef tn' (Just [dtor])
    insertDtor _ refs = refs

    matchType :: P.ProperName 'P.TypeName -> P.DeclarationRef -> Bool
    matchType tn (P.TypeRef n _) = tn == n
    matchType _ _ = False

updateAtFirstOrPrepend :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
updateAtFirstOrPrepend p t d l =
  case List.findIndex p l of
    Nothing -> d : l
    Just ix ->
      let (x, a : y) = List.splitAt ix l
      in t a : x ++ y

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
                          -> m (Either [Match] [Text])
addImportForIdentifier fp ident filters = do
  modules <- getAllModulesWithReexports
  case getExactMatches ident filters modules of
    [] ->
      throwError (NotFound "Couldn't find the given identifier. \
                           \Have you loaded the corresponding module?")

    -- Only one match was found for the given identifier, so we can insert it
    -- right away
    [Match m decl] ->
      Right <$> addExplicitImport fp decl (P.moduleNameFromString (T.unpack m))

    -- This case comes up for newtypes and dataconstructors. Because values and
    -- types don't share a namespace we can get multiple matches from the same
    -- module.

    ms@[Match m1 d1, Match m2 d2] ->
      if m1 /= m2
         -- If the modules don't line up we just ask the user to specify the
         -- module
      then pure $ Left ms
      else case dtorAndTypeMatch d1 d2 of
        -- If dataconstructor and type line up we just import the
        -- dataconstructor as that will give us an unnecessary import warning at
        -- worst
        Just dtor ->
          Right <$> addExplicitImport fp dtor (P.moduleNameFromString (T.unpack m1))
        -- Here we need the user to specify whether he wanted a dataconstructor
        -- or a type
        Nothing ->
          throwError (GeneralError "Undecidable between type and dataconstructor")

    -- Multiple matches were found so we need to ask the user to clarify which
    -- module he meant
    xs ->
      pure $ Left xs
    where
      dtorAndTypeMatch dtor@(DataConstructor _ t _) (TypeDeclaration t' _) =
        if t == t' then Just dtor else Nothing
      dtorAndTypeMatch (TypeDeclaration t' _) dtor@(DataConstructor _ t _) =
        if t' == t then Just dtor else Nothing
      dtorAndTypeMatch _ _ = Nothing

prettyPrintImport' :: Import -> Text
-- TODO: remove this clause once P.prettyPrintImport can properly handle PositionedRefs
prettyPrintImport' (Import mn (P.Explicit refs) qual) =
  T.pack $ "import " ++ P.prettyPrintImport mn (P.Explicit (unwrapPositionedRef <$> refs)) qual
prettyPrintImport' (Import mn idt qual) =
  T.pack $ "import " ++ P.prettyPrintImport mn idt qual

prettyPrintImportSection :: [Import] -> [Text]
prettyPrintImportSection = List.sort . map prettyPrintImport'

-- | Writes a list of lines to @Just filepath@ and responds with a @TextResult@,
-- or returns the lines as a @MultilineTextResult@ if @Nothing@ was given as the
-- first argument.
answerRequest :: (MonadIO m) => Maybe FilePath -> [Text] -> m Success
answerRequest outfp rs  =
  case outfp of
    Nothing -> pure $ MultilineTextResult rs
    Just outfp' -> do
      liftIO $ TIO.writeFile outfp' (T.unlines rs)
      pure $ TextResult $ "Written to " <> T.pack outfp'

-- | Test and ghci helper
parseImport :: Text -> Maybe Import
parseImport t =
  case P.lex "<psc-ide>" (T.unpack t)
       >>= P.runTokenParser "<psc-ide>" P.parseImportDeclaration' of
    Right (mn, P.Explicit refs, mmn, _) ->
      Just (Import mn (P.Explicit (unwrapPositionedRef <$> refs)) mmn)
    Right (mn, idt, mmn, _) -> Just (Import mn idt mmn)
    Left _ -> Nothing

