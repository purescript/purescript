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

{-# LANGUAGE OverloadedStrings     #-}

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

import           Protolude

import           Control.Lens                       ((^.))
import           Data.List                          (findIndex, nubBy)
import qualified Data.Text                          as T
import qualified Language.PureScript                as P
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.IO.UTF8                     (readUTF8FileT, writeUTF8FileT)

data Import = Import P.ModuleName P.ImportDeclarationType  (Maybe P.ModuleName)
              deriving (Eq, Show)

instance Ord Import where
  compare = compImport

compImportType :: P.ImportDeclarationType -> P.ImportDeclarationType -> Ordering
compImportType P.Implicit P.Implicit = EQ
compImportType P.Implicit _ = LT
compImportType (P.Explicit _) (P.Hiding _) = LT
compImportType (P.Explicit _) (P.Explicit _) = EQ
compImportType (P.Explicit _) P.Implicit = GT
compImportType (P.Hiding _) (P.Hiding _) = EQ
compImportType (P.Hiding _) _ = GT

compImport :: Import -> Import -> Ordering
compImport (Import n i q) (Import n' i' q')
  | compImportType i i' /= EQ = compImportType i i'
    -- This means that for a stable sort, the first implicit import will stay
    -- the first implicit import
  | not (P.isExplicit i) && isNothing q = LT
  | not (P.isExplicit i) && isNothing q' = GT
  | otherwise = compare n n'

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                        FilePath -> m (P.ModuleName, [Text], [Import], [Text])
parseImportsFromFile fp = do
  file <- liftIO (readUTF8FileT fp)
  case sliceImportSection (T.lines file) of
    Right res -> pure res
    Left err -> throwError (GeneralError err)

parseImportsWithModuleName :: [Text] -> Either Text (P.ModuleName, [Import])
parseImportsWithModuleName ls = do
  (P.Module _ _ mn decls _) <- moduleParse ls
  pure (mn, concatMap mkImport (unwrapPositioned <$> decls))
  where
    mkImport (P.ImportDeclaration mn (P.Explicit refs) qual) =
      [Import mn (P.Explicit (unwrapPositionedRef <$> refs)) qual]
    mkImport (P.ImportDeclaration mn it qual) = [Import mn it qual]
    mkImport _ = []

sliceImportSection :: [Text] -> Either Text (P.ModuleName, [Text], [Import], [Text])
sliceImportSection ts =
  case foldl step (ModuleHeader 0) (zip [0..] ts) of
    Res start end ->
      let
        (moduleHeader, (importSection, remainingFile)) =
          splitAt (succ (end - start)) `second` splitAt start ts
      in
        (\(mn, is) -> (mn, moduleHeader, is, remainingFile)) <$>
          parseImportsWithModuleName (moduleHeader <> importSection)

    -- If we don't find any imports, we insert a newline after the module
    -- declaration and begin a new importsection
    ModuleHeader ix ->
      let (moduleHeader, remainingFile) = splitAt (succ ix) ts
      in
        (\(mn, is) -> (mn, moduleHeader ++ [""], is, remainingFile)) <$>
          parseImportsWithModuleName moduleHeader
    _ -> Left "Failed to detect the import section"

data ImportStateMachine = ModuleHeader Int | ImportSection Int Int | Res Int Int

-- | We start in the
--
-- * ModuleHeader state.
--
-- We skip every line we encounter, that doesn't start with "import". If we find
-- a line that starts with module we store that linenumber. Once we find a line
-- with "import" we store its linenumber as the start of the import section and
-- change into the
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
step (ModuleHeader mi) (ix, l)
  | T.isPrefixOf "module " l = ModuleHeader ix
  | T.isPrefixOf "import " l = ImportSection ix ix
  | otherwise = ModuleHeader mi
step (ImportSection start lastImportLine) (ix, l)
  | any (`T.isPrefixOf` l) ["import", " "] = ImportSection start ix
  | T.isPrefixOf "--" l || l == ""         = ImportSection start lastImportLine
  | otherwise                              = Res start lastImportLine
step (Res start end) _ = Res start end

moduleParse :: [Text] -> Either Text P.Module
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
  -- We need to append the new import, because there could already be implicit
  -- imports and we need to preserve the order on these, as the first implicit
  -- import is the one that doesn't generate warnings.
  prettyPrintImportSection ( imports ++ [Import mn P.Implicit Nothing])

-- | Adds an explicit import like @import Prelude (unit)@ to a Sourcefile. If an
-- explicit import already exists for the given module, it adds the identifier
-- to that imports list.
--
-- So @addExplicitImport "/File.purs" "bind" "Prelude"@ with an already existing
-- @import Prelude (bind)@ in the file File.purs returns @["import Prelude
-- (bind, unit)"]@
addExplicitImport :: (MonadIO m, MonadError PscIdeError m) =>
                     FilePath -> IdeDeclaration -> P.ModuleName -> m [Text]
addExplicitImport fp decl moduleName = do
  (mn, pre, imports, post) <- parseImportsFromFile fp
  let newImportSection =
        -- TODO: Open an issue when this PR is merged, we should optimise this
        -- so that this case does not write to disc
        if mn == moduleName
        then imports
        else addExplicitImport' decl moduleName imports
  pure (pre ++ prettyPrintImportSection newImportSection ++ post)

addExplicitImport' :: IdeDeclaration -> P.ModuleName -> [Import] -> [Import]
addExplicitImport' decl moduleName imports =
  let
    isImplicitlyImported =
      not . null $ filter (\case
                              (Import mn P.Implicit Nothing) -> mn == moduleName
                              _ -> False) imports
    matches (Import mn (P.Explicit _) Nothing) = mn == moduleName
    matches _ = False
    freshImport = Import moduleName (P.Explicit [refFromDeclaration decl]) Nothing
  in
    if isImplicitlyImported
    then imports
    else updateAtFirstOrPrepend matches (insertDeclIntoImport decl) freshImport imports
  where
    refFromDeclaration (IdeDeclTypeClass n) =
      P.TypeClassRef n
    refFromDeclaration (IdeDeclDataConstructor dtor) =
      P.TypeRef (dtor ^. ideDtorTypeName) Nothing
    refFromDeclaration (IdeDeclType t) =
      P.TypeRef (t ^. ideTypeName) (Just [])
    refFromDeclaration (IdeDeclValueOperator op) =
      P.ValueOpRef (op ^. ideValueOpName)
    refFromDeclaration (IdeDeclTypeOperator op) =
      P.TypeOpRef (op ^. ideTypeOpName)
    refFromDeclaration d =
      P.ValueRef $ P.Ident $ T.unpack (identifierFromIdeDeclaration d)

    -- | Adds a declaration to an import:
    -- TypeDeclaration "Maybe" + Data.Maybe (maybe) -> Data.Maybe(Maybe, maybe)
    insertDeclIntoImport :: IdeDeclaration -> Import -> Import
    insertDeclIntoImport decl' (Import mn (P.Explicit refs) Nothing) =
      Import mn (P.Explicit (sortBy P.compDecRef (insertDeclIntoRefs decl' refs))) Nothing
    insertDeclIntoImport _ is = is

    insertDeclIntoRefs :: IdeDeclaration -> [P.DeclarationRef] -> [P.DeclarationRef]
    insertDeclIntoRefs d@(IdeDeclDataConstructor dtor) refs =
      updateAtFirstOrPrepend
        (matchType (dtor ^. ideDtorTypeName))
        (insertDtor (dtor ^. ideDtorName))
        (refFromDeclaration d)
        refs
    insertDeclIntoRefs dr refs = nubBy ((==) `on` P.prettyPrintRef) (refFromDeclaration dr : refs)

    insertDtor _ (P.TypeRef tn' _) = P.TypeRef tn' Nothing
    insertDtor _ refs = refs

    matchType :: P.ProperName 'P.TypeName -> P.DeclarationRef -> Bool
    matchType tn (P.TypeRef n _) = tn == n
    matchType _ _ = False

updateAtFirstOrPrepend :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
updateAtFirstOrPrepend p t d l =
  case findIndex p l of
    Nothing -> d : l
    Just ix ->
      let (x, a : y) = splitAt ix l
      in x ++ [t a] ++ y

-- | Looks up the given identifier in the currently loaded modules.
--
-- * Throws an error if the identifier cannot be found.
--
-- * If exactly one match is found, adds an explicit import to the importsection
--
-- * If more than one possible imports are found, reports the possibilities as a
-- list of completions.
addImportForIdentifier :: (Ide m, MonadError PscIdeError m)
                          => FilePath -- ^ The Sourcefile to read from
                          -> Text     -- ^ The identifier to import
                          -> [Filter] -- ^ Filters to apply before searching for
                                      -- the identifier
                          -> m (Either [Match IdeDeclaration] [Text])
addImportForIdentifier fp ident filters = do
  modules <- getAllModules Nothing
  case map (fmap discardAnn) (getExactMatches ident filters modules) of
    [] ->
      throwError (NotFound "Couldn't find the given identifier. \
                           \Have you loaded the corresponding module?")

    -- Only one match was found for the given identifier, so we can insert it
    -- right away
    [Match (m, decl)] ->
      Right <$> addExplicitImport fp decl m

    -- This case comes up for newtypes and dataconstructors. Because values and
    -- types don't share a namespace we can get multiple matches from the same
    -- module. This also happens for parameterized types, as these generate both
    -- a type aswell as a type synonym.

    ms@[Match (m1, d1), Match (m2, d2)] ->
      if m1 /= m2
         -- If the modules don't line up we just ask the user to specify the
         -- module
      then pure $ Left ms
      else case decideRedundantCase d1 d2 <|> decideRedundantCase d2 d1 of
        -- If dataconstructor and type line up we just import the
        -- dataconstructor as that will give us an unnecessary import warning at
        -- worst
        Just decl ->
          Right <$> addExplicitImport fp decl m1
        -- Here we need the user to specify whether he wanted a dataconstructor
        -- or a type
        Nothing ->
          throwError (GeneralError "Undecidable between type and dataconstructor")

    -- Multiple matches were found so we need to ask the user to clarify which
    -- module he meant
    xs ->
      pure $ Left xs
    where
      decideRedundantCase d@(IdeDeclDataConstructor dtor) (IdeDeclType t) =
        if dtor ^. ideDtorTypeName == t ^. ideTypeName then Just d else Nothing
      decideRedundantCase IdeDeclType{} ts@IdeDeclTypeSynonym{} =
        Just ts
      decideRedundantCase _ _ = Nothing

prettyPrintImport' :: Import -> Text
-- TODO: remove this clause once P.prettyPrintImport can properly handle PositionedRefs
prettyPrintImport' (Import mn (P.Explicit refs) qual) =
  T.pack $ "import " ++ P.prettyPrintImport mn (P.Explicit (unwrapPositionedRef <$> refs)) qual
prettyPrintImport' (Import mn idt qual) =
  T.pack $ "import " ++ P.prettyPrintImport mn idt qual

prettyPrintImportSection :: [Import] -> [Text]
prettyPrintImportSection imports = map prettyPrintImport' (sort imports)

-- | Writes a list of lines to @Just filepath@ and responds with a @TextResult@,
-- or returns the lines as a @MultilineTextResult@ if @Nothing@ was given as the
-- first argument.
answerRequest :: (MonadIO m) => Maybe FilePath -> [Text] -> m Success
answerRequest outfp rs  =
  case outfp of
    Nothing -> pure (MultilineTextResult rs)
    Just outfp' -> do
      liftIO (writeUTF8FileT outfp' (T.unlines rs))
      pure (TextResult ("Written to " <> T.pack outfp'))

-- | Test and ghci helper
parseImport :: Text -> Maybe Import
parseImport t =
  case P.lex "<psc-ide>" (T.unpack t)
       >>= P.runTokenParser "<psc-ide>" P.parseImportDeclaration' of
    Right (mn, P.Explicit refs, mmn) ->
      Just (Import mn (P.Explicit (unwrapPositionedRef <$> refs)) mmn)
    Right (mn, idt, mmn) -> Just (Import mn idt mmn)
    Left _ -> Nothing
