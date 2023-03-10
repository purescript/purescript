module Language.PureScript.Ide.Imports.Actions 
       ( addImplicitImport
       , addQualifiedImport
       , addImportForIdentifier
       , answerRequest

        -- for tests
       , addImplicitImport'
       , addQualifiedImport'
       , addExplicitImport'
       )
where

import Protolude hiding (moduleName)

import Control.Lens ((^.), has)
import Data.List (nubBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Ide.Completion
import Language.PureScript.Ide.Error
import Language.PureScript.Ide.Filter
import Language.PureScript.Ide.Imports
import Language.PureScript.Ide.State
import Language.PureScript.Ide.Prim
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Util
import System.IO.UTF8 (writeUTF8FileT)

-- | Adds an implicit import like @import Prelude@ to a Sourcefile.
addImplicitImport
  :: (MonadIO m, MonadError IdeError m)
  => FilePath     -- ^ The source file read from
  -> P.ModuleName -- ^ The module to import
  -> m [Text]
addImplicitImport fp mn = do
  (_, pre, imports, post) <- parseImportsFromFile' fp
  let newImportSection = addImplicitImport' imports mn
  pure $ joinSections (pre, newImportSection, post)

addImplicitImport' :: [Import] -> P.ModuleName -> [Text]
addImplicitImport' imports mn =
  prettyPrintImportSection (Import mn P.Implicit Nothing : imports)

-- | Adds a qualified import like @import Data.Map as Map@ to a source file.
addQualifiedImport
  :: (MonadIO m, MonadError IdeError m)
  => FilePath
  -- ^ The sourcefile read from
  -> P.ModuleName
  -- ^ The module to import
  -> P.ModuleName
  -- ^ The qualifier under which to import
  -> m [Text]
addQualifiedImport fp mn qualifier = do
  (_, pre, imports, post) <- parseImportsFromFile' fp
  let newImportSection = addQualifiedImport' imports mn qualifier
  pure $ joinSections (pre, newImportSection, post)

addQualifiedImport' :: [Import] -> P.ModuleName -> P.ModuleName -> [Text]
addQualifiedImport' imports mn qualifier =
  prettyPrintImportSection (Import mn P.Implicit (Just qualifier) : imports)

-- | Adds an explicit import like @import Prelude (unit)@ to a Sourcefile. If an
-- explicit import already exists for the given module, it adds the identifier
-- to that imports list.
--
-- So @addExplicitImport "/File.purs" "bind" "Prelude"@ with an already existing
-- @import Prelude (bind)@ in the file File.purs returns @["import Prelude
-- (bind, unit)"]@
addExplicitImport :: (MonadIO m, MonadError IdeError m) =>
                     FilePath -> IdeDeclaration -> P.ModuleName -> Maybe P.ModuleName -> m [Text]
addExplicitImport fp decl moduleName qualifier = do
  (mn, pre, imports, post) <- parseImportsFromFile' fp
  let newImportSection =
        -- TODO: Open an issue when this PR is merged, we should optimise this
        -- so that this case does not write to disc
        if mn == moduleName
        then imports
        else addExplicitImport' decl moduleName qualifier imports
  pure $ joinSections (pre, prettyPrintImportSection newImportSection, post)

addExplicitImport' :: IdeDeclaration -> P.ModuleName -> Maybe P.ModuleName -> [Import] -> [Import]
addExplicitImport' decl moduleName qualifier imports =
  let
    isImplicitlyImported =
        any (\case
          Import mn P.Implicit qualifier' -> mn == moduleName && qualifier == qualifier'
          _ -> False) imports
    isNotExplicitlyImportedFromPrim =
      moduleName == C.M_Prim &&
        not (any (\case
          Import C.M_Prim (P.Explicit _) Nothing -> True
          _ -> False) imports)
    -- We can't import Modules from other modules
    isModule = has _IdeDeclModule decl

    matches (Import mn (P.Explicit _) qualifier') = mn == moduleName && qualifier == qualifier'
    matches _ = False
    freshImport = Import moduleName (P.Explicit [refFromDeclaration decl]) qualifier
  in
    if isImplicitlyImported || isNotExplicitlyImportedFromPrim || isModule
    then imports
    else updateAtFirstOrPrepend matches (insertDeclIntoImport decl) freshImport imports
  where
    refFromDeclaration (IdeDeclTypeClass tc) =
      P.TypeClassRef ideSpan (tc ^. ideTCName)
    refFromDeclaration (IdeDeclDataConstructor dtor) =
      P.TypeRef ideSpan (dtor ^. ideDtorTypeName) Nothing
    refFromDeclaration (IdeDeclType t) =
      P.TypeRef ideSpan (t ^. ideTypeName) (Just [])
    refFromDeclaration (IdeDeclValueOperator op) =
      P.ValueOpRef ideSpan (op ^. ideValueOpName)
    refFromDeclaration (IdeDeclTypeOperator op) =
      P.TypeOpRef ideSpan (op ^. ideTypeOpName)
    refFromDeclaration d =
      P.ValueRef ideSpan (P.Ident (identifierFromIdeDeclaration d))

    -- | Adds a declaration to an import:
    -- TypeDeclaration "Maybe" + Data.Maybe (maybe) -> Data.Maybe(Maybe, maybe)
    insertDeclIntoImport :: IdeDeclaration -> Import -> Import
    insertDeclIntoImport decl' (Import mn (P.Explicit refs) qual) =
      Import mn (P.Explicit (sort (insertDeclIntoRefs decl' refs))) qual
    insertDeclIntoImport _ is = is

    insertDeclIntoRefs :: IdeDeclaration -> [P.DeclarationRef] -> [P.DeclarationRef]
    insertDeclIntoRefs d@(IdeDeclDataConstructor dtor) refs =
      updateAtFirstOrPrepend
        (matchType (dtor ^. ideDtorTypeName))
        (insertDtor (dtor ^. ideDtorName))
        (refFromDeclaration d)
        refs
    insertDeclIntoRefs (IdeDeclType t) refs
      | any matches refs = refs
      where
        matches (P.TypeRef _ typeName _) = _ideTypeName t == typeName
        matches _ = False
    insertDeclIntoRefs dr refs = nubBy ((==) `on` P.prettyPrintRef) (refFromDeclaration dr : refs)

    insertDtor _ (P.TypeRef ss tn' _) = P.TypeRef ss tn' Nothing
    insertDtor _ refs = refs

    matchType :: P.ProperName 'P.TypeName -> P.DeclarationRef -> Bool
    matchType tn (P.TypeRef _ n _) = tn == n
    matchType _ _ = False


-- | Looks up the given identifier in the currently loaded modules.
--
-- * Throws an error if the identifier cannot be found.
--
-- * If exactly one match is found, adds an explicit import to the importsection
--
-- * If more than one possible imports are found, reports the possibilities as a
-- list of completions.
addImportForIdentifier
  :: (Ide m, MonadError IdeError m)
  => FilePath -- ^ The Sourcefile to read from
  -> Text     -- ^ The identifier to import
  -> Maybe P.ModuleName  -- ^ The optional qualifier under which to import
  -> [Filter] -- ^ Filters to apply before searching for the identifier
  -> m (Either [Match IdeDeclaration] [Text])
addImportForIdentifier fp ident qual filters = do
  let addPrim = Map.union idePrimDeclarations
  modules <- getAllModules Nothing
  let
    matches =
      getExactMatches ident filters (addPrim modules)
        & map (fmap discardAnn)
        & filter (\(Match (_, d)) -> not (has _IdeDeclModule d))

  case matches of
    [] ->
      throwError (NotFound "Couldn't find the given identifier. \
                           \Have you loaded the corresponding module?")

    -- Only one match was found for the given identifier, so we can insert it
    -- right away
    [Match (m, decl)] ->
      Right <$> addExplicitImport fp decl m qual

    -- This case comes up for newtypes and dataconstructors. Because values and
    -- types don't share a namespace we can get multiple matches from the same
    -- module. This also happens for parameterized types, as these generate both
    -- a type as well as a type synonym.

    ms@[Match (m1, d1), Match (m2, d2)] ->
      if m1 /= m2
         -- If the modules don't line up we just ask the user to specify the
         -- module
      then pure (Left ms)
      else case decideRedundantCase d1 d2 <|> decideRedundantCase d2 d1 of
        -- If dataconstructor and type line up we just import the
        -- dataconstructor as that will give us an unnecessary import warning at
        -- worst
        Just decl ->
          Right <$> addExplicitImport fp decl m1 qual
        -- Here we need the user to specify whether they wanted a 
        -- dataconstructor or a type
        Nothing ->
          throwError (GeneralError "Undecidable between type and dataconstructor")

    -- Multiple matches were found so we need to ask the user to clarify which
    -- module they meant
    xs ->
      pure (Left xs)
    where
      decideRedundantCase d@(IdeDeclDataConstructor dtor) (IdeDeclType t) =
        if dtor ^. ideDtorTypeName == t ^. ideTypeName then Just d else Nothing
      decideRedundantCase IdeDeclType{} ts@IdeDeclTypeSynonym{} =
        Just ts
      decideRedundantCase _ _ = Nothing

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


-- | If none of the elements of the list satisfy the given predicate 'predicate', then prepend the default value 'def'
-- to the given list. Otherwise, update the first element of the list that satisfies 'predicate' with the updating
-- function 'update'.
updateAtFirstOrPrepend :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
updateAtFirstOrPrepend predicate update def xs =
  case break predicate xs of
    (before, []) -> def : before
    (before, x : after) -> before ++ [update x] ++ after


ideSpan :: P.SourceSpan
ideSpan = P.internalModuleSourceSpan "<psc-ide>"

joinSections :: ([Text], [Text], [Text]) -> [Text]
joinSections (pre, decls, post) = pre `joinLine` (decls `joinLine` post)
  where
  isBlank = T.all (== ' ')
  joinLine as bs
    | Just ln1 <- lastMay as
    , Just ln2 <- head bs
    , not (isBlank ln1) && not (isBlank ln2) =
        as ++ [""] ++ bs
    | otherwise =
        as ++ bs
