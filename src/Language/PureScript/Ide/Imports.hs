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

module Language.PureScript.Ide.Imports
       ( addImplicitImport
       , addImportForIdentifier
       , answerRequest
       , parseImportsFromFile
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

import           Control.Lens                       ((^.), (%~), ix)
import           Data.List                          (findIndex, nubBy)
import qualified Data.Text                          as T
import qualified Language.PureScript                as P
import           Language.PureScript.Ide.Completion
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.IO.UTF8                     (writeUTF8FileT)
import qualified Text.Parsec as Parsec

data Import = Import P.ModuleName P.ImportDeclarationType (Maybe P.ModuleName)
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

-- | Reads a file and returns the parsed modulename as well as the parsed
-- imports, while ignoring eventual parse errors that aren't relevant to the
-- import section
parseImportsFromFile
  :: (MonadIO m, MonadError IdeError m)
  => FilePath
  -> m (P.ModuleName, [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)])
parseImportsFromFile file = do
  (mn, _, imports, _) <- parseImportsFromFile' file
  pure (mn, unwrapImport <$> imports)
  where
    unwrapImport (Import a b c) = (a, b, c)

-- | Reads a file and returns the (lines before the imports, the imports, the
-- lines after the imports)
parseImportsFromFile' :: (MonadIO m, MonadError IdeError m) =>
                        FilePath -> m (P.ModuleName, [Text], [Import], [Text])
parseImportsFromFile' fp = do
  file <- ideReadFile fp
  case sliceImportSection (T.lines file) of
    Right res -> pure res
    Left err -> throwError (GeneralError err)

-- | @ImportParse@ holds the data we extract out of a partial parse of the
-- sourcefile
data ImportParse = ImportParse
  { ipModuleName :: P.ModuleName
  -- ^ the module name we parse
  , ipStart :: P.SourcePos
  -- ^ the beginning of the import section. If `import Prelude` was the first
  -- import, this would point at `i`
  , ipEnd :: P.SourcePos
  -- ^ the end of the import section
  , ipImports :: [Import]
  -- ^ the extracted import declarations
  }

parseModuleHeader :: P.TokenParser ImportParse
parseModuleHeader = do
  _ <- P.readComments
  (mn, _) <- P.parseModuleDeclaration
  (ipStart, ipEnd, decls) <- P.withSourceSpan (\(P.SourceSpan _ start end) _ -> (start, end,))
    (P.mark (Parsec.many (P.same *> P.parseImportDeclaration')))
  pure (ImportParse mn ipStart ipEnd (map mkImport decls))
  where
    mkImport (mn, (P.Explicit refs), qual) = Import mn (P.Explicit (unwrapPositionedRef <$> refs)) qual
    mkImport (mn, it, qual) = Import mn it qual

sliceImportSection :: [Text] -> Either Text (P.ModuleName, [Text], [Import], [Text])
sliceImportSection fileLines = first show $ do
  tokens <- P.lexLenient "<psc-ide>" file
  ImportParse{..} <- P.runTokenParser "<psc-ide>" parseModuleHeader tokens
  pure ( ipModuleName
       , sliceFile (P.SourcePos 1 1) (prevPos ipStart)
       , ipImports
       -- Not sure why I need to drop 1 here, but it makes the tests pass
       , drop 1 (sliceFile (nextPos ipEnd) (P.SourcePos (length fileLines) (lineLength (length fileLines))))
       )
  where
    prevPos (P.SourcePos l c)
      | l == 1 && c == 1 = P.SourcePos l c
      | c == 1 = P.SourcePos (l - 1) (lineLength (l - 1))
      | otherwise = P.SourcePos l (c - 1)
    nextPos (P.SourcePos l c)
      | c == lineLength l = P.SourcePos (l + 1) 1
      | otherwise = P.SourcePos l (c + 1)
    file = T.unlines fileLines
    lineLength l = T.length (fileLines ^. ix (l - 1))
    sliceFile (P.SourcePos l1 c1) (P.SourcePos l2 c2) =
      fileLines
      & drop (l1 - 1)
      & take (l2 - l1 + 1)
      & ix 0 %~ T.drop (c1 - 1)
      & ix (l2 - l1) %~ T.take c2

-- | Adds an implicit import like @import Prelude@ to a Sourcefile.
addImplicitImport :: (MonadIO m, MonadError IdeError m)
                     => FilePath     -- ^ The Sourcefile read from
                     -> P.ModuleName -- ^ The module to import
                     -> m [Text]
addImplicitImport fp mn = do
  (_, pre, imports, post) <- parseImportsFromFile' fp
  let newImportSection = addImplicitImport' imports mn
  pure (pre ++ newImportSection ++ post)

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
addExplicitImport :: (MonadIO m, MonadError IdeError m) =>
                     FilePath -> IdeDeclaration -> P.ModuleName -> m [Text]
addExplicitImport fp decl moduleName = do
  (mn, pre, imports, post) <- parseImportsFromFile' fp
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
    refFromDeclaration (IdeDeclTypeClass tc) =
      P.TypeClassRef (tc ^. ideTCName)
    refFromDeclaration (IdeDeclDataConstructor dtor) =
      P.TypeRef (dtor ^. ideDtorTypeName) Nothing
    refFromDeclaration (IdeDeclType t) =
      P.TypeRef (t ^. ideTypeName) (Just [])
    refFromDeclaration (IdeDeclValueOperator op) =
      P.ValueOpRef (op ^. ideValueOpName)
    refFromDeclaration (IdeDeclTypeOperator op) =
      P.TypeOpRef (op ^. ideTypeOpName)
    refFromDeclaration (IdeDeclKind kn) =
      P.KindRef kn
    refFromDeclaration d =
      P.ValueRef (P.Ident (identifierFromIdeDeclaration d))

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
    Just i ->
      let (x, a : y) = splitAt i l
      in x ++ [t a] ++ y

-- | Looks up the given identifier in the currently loaded modules.
--
-- * Throws an error if the identifier cannot be found.
--
-- * If exactly one match is found, adds an explicit import to the importsection
--
-- * If more than one possible imports are found, reports the possibilities as a
-- list of completions.
addImportForIdentifier :: (Ide m, MonadError IdeError m)
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
prettyPrintImport' (Import mn idt qual) =
  "import " <> P.prettyPrintImport mn idt qual

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
  case P.lex "<psc-ide>" t
       >>= P.runTokenParser "<psc-ide>" P.parseImportDeclaration' of
    Right (mn, P.Explicit refs, mmn) ->
      Just (Import mn (P.Explicit (unwrapPositionedRef <$> refs)) mmn)
    Right (mn, idt, mmn) -> Just (Import mn idt mmn)
    Left _ -> Nothing
