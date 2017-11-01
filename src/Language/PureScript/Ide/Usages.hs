module Language.PureScript.Ide.Usages where

import           Protolude

import           Control.Lens ((^.), over)
import qualified Data.Map as Map
import qualified Language.PureScript as P
import           Language.PureScript.Ide.Imports.Helpers
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

-- | The usage of an identifier inside some other declaration
data Usage = Usage
  { usageSiteModule :: P.ModuleName
  , usageSiteLocation :: P.SourceSpan
  , usageOriginId :: IdeDeclarationId
  } deriving (Show)

showUsage :: Usage -> Text
showUsage Usage{..} =
  P.runModuleName (usageOriginId ^. ididModule)
  <> "."
  <> usageOriginId ^. ididIdentifier
  <> " used in: "
  <> P.runModuleName usageSiteModule
  <> " "
  <> P.displaySourceSpan "." usageSiteLocation

-- | Collects all usages of names for a given module
collectUsages :: P.Module -> [Usage]
collectUsages m@(P.Module _ _ mn decls _) =
  let
    freeNames = foldMap collectFreeNames decls
    topLevelNames = collectTopLevelValues m
    imports = mapMaybe toIdeImport decls
    importUsages = foldMap (getUsagesFromImport mn) imports
  in
    importUsages <> (flip mapMaybe freeNames $ \n@(ns, ident, ss) -> do
      resolvedModule <- resolveFreeName imports topLevelNames mn n
      pure $ Usage mn ss $ IdeDeclarationId resolvedModule ns $ P.runIdent (P.disqualify ident))

getUsagesFromImport :: P.ModuleName -> Import -> [Usage]
getUsagesFromImport usageModule (Import originModule it _) = case it of
  P.Explicit refs -> mapMaybe usagesFromRef refs
   -- TODO(Christoph): When we start collecting references to module names,
   -- we'll need to handle this case
  _ -> []
  where
    mkUsage ss ns ident = Just (Usage usageModule ss (IdeDeclarationId originModule ns ident))
    usagesFromRef = \case
      P.TypeRef ss tn _ctors ->
        --TODO(Christoph): This means we're currently throwing away usages for
        --explicitly imported dtors. For a proper rename refactoring this will
        --need to change, but the current AST format doesn't record the
        --necessary SourceSpans anyway
        mkUsage ss IdeNSType (P.runProperName tn)
      P.TypeOpRef ss opname ->
        mkUsage ss IdeNSType (P.runOpName opname)
      P.ValueRef ss ident ->
        mkUsage ss IdeNSValue (P.runIdent ident)
      P.ValueOpRef ss opname ->
        mkUsage ss IdeNSValue (P.runOpName opname)
      P.TypeClassRef ss className ->
        mkUsage ss IdeNSType (P.runProperName className)
      P.TypeInstanceRef ss ident ->
        mkUsage ss IdeNSValue (P.runIdent ident)
      P.KindRef ss name ->
        mkUsage ss IdeNSKind (P.runProperName name)
      P.ReExportRef{} -> Nothing
      -- Module references can never appear inside an import declaration
      P.ModuleRef{} -> Nothing

resolveFreeName
  :: [Import]
  -> [P.Ident]
  -> P.ModuleName
  -> (IdeNamespace, P.Qualified P.Ident, P.SourceSpan)
  -> Maybe P.ModuleName
resolveFreeName imports topLevelNames currentModule name = case name of
  -- Non-qualified values
  (IdeNSValue, P.Qualified Nothing i, _) ->
    -- Is the value defined in this module?
    (guard (elem i topLevelNames) $> currentModule)
    -- Is the value imported explicitly?
    <|> head (mapMaybe (resolveAgainstExplicitImport i) imports)
    -- Falling back to implicit imports
    <|> resolveAgainstSingleImplicitImport i imports
  (IdeNSValue, P.Qualified (Just qualifier) i, _) ->
     head (mapMaybe (resolveAgainstQualifiedImport qualifier i) imports)
  _ ->
    -- TODO(Christoph): Handle more cases
    Nothing

resolveAgainstQualifiedImport :: P.ModuleName -> P.Ident -> Import ->  Maybe P.ModuleName
resolveAgainstQualifiedImport qual ident = \case
  Import mn it (Just qual') | qual == qual' ->
    case it of
      P.Implicit -> Just mn
      P.Explicit refs -> guard (any (refMatchesIdent ident) refs) $> mn
      P.Hiding refs -> guard (not (any (refMatchesIdent ident) refs)) $> mn
  _ ->
    Nothing

resolveAgainstSingleImplicitImport :: P.Ident -> [Import] -> Maybe P.ModuleName
resolveAgainstSingleImplicitImport ident imports = case mapMaybe isImplicit imports of
  [implicit] -> Just implicit
  _ -> Nothing
  where
    isImplicit = \case
      Import mn P.Implicit Nothing -> Just mn
      Import mn (P.Hiding refs) Nothing -> guard (not (any (refMatchesIdent ident) refs)) $> mn
      _ -> Nothing

resolveAgainstExplicitImport :: P.Ident -> Import -> Maybe P.ModuleName
resolveAgainstExplicitImport ident i = case i of
  Import mn (P.Explicit refs) Nothing ->
    guard (any (refMatchesIdent ident) refs) $> mn
  _ -> Nothing

refMatchesIdent :: P.Ident -> P.DeclarationRef -> Bool
refMatchesIdent ident = \case
  P.ValueRef _ refIdent -> refIdent == ident
  _ -> False

toIdeImport :: P.Declaration -> Maybe Import
toIdeImport d = case d of
  P.ImportDeclaration _ mn idt qualifier ->
    Just (Import mn idt qualifier)
  _ -> Nothing

collectTopLevelValues :: P.Module -> [P.Ident]
collectTopLevelValues =
  mapMaybe (map P.valdeclIdent . P.getValueDeclaration) . P.getModuleDeclarations

collectFreeNames :: P.Declaration -> [(IdeNamespace, P.Qualified P.Ident, P.SourceSpan)]
collectFreeNames =
  let
    (extractor, _, _, _, _) =
      P.everythingWithScope
        mempty
        exprs
        mempty
        mempty
        mempty
  in extractor mempty
  where
    exprs scope expr = case expr of
      P.PositionedValue ss _ (P.Var v)
        | P.Qualified Nothing i <- v
        , not (i `elem` scope) -> [(IdeNSValue, v, ss)]
      P.PositionedValue ss _ (P.Var v) ->
        [(IdeNSValue, v, ss)]
      _ -> []

insertUsage
  :: Usage
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
insertUsage Usage{..} decls =
  modifyAtDeclarationId usageId (over (idaAnnotation.annUsages) updateUsages) decls
  where
    -- We first need to check if the Usage we found is actually a reexport
    reexportedFrom = do
      ds <- Map.lookup (usageOriginId ^. ididModule) decls
      d <- find (declarationMatchesId usageOriginId) ds
      d ^. idaAnnotation.annExportedFrom

    usageId = case reexportedFrom of
      Just originalModule ->
        -- If we're looking at a reexport, we associate the usage with the
        -- defining module instead
        usageOriginId { _ididModule = originalModule }
      Nothing ->
        usageOriginId

    updateUsages = \case
      Nothing ->
        Just [(usageSiteModule, usageSiteLocation)]
      Just prev ->
        Just ((usageSiteModule, usageSiteLocation) : prev)

-- | Collects uses in all the parsed modules and inserts them into the
-- Annotation for the originating declaration. Expects Reexports to have been
-- resolved beforehand, so it can associate a usage with the original
-- declaration.
resolveUsages
  :: ModuleMap P.Module
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
resolveUsages modules decls =
  foldr resolveUsagesForModule decls modules
  where
    resolveUsagesForModule module' decls' =
      foldr insertUsage decls' (collectUsages module')

modifyAtDeclarationId
  :: IdeDeclarationId
  -> (IdeDeclarationAnn -> IdeDeclarationAnn)
  -> ModuleMap [IdeDeclarationAnn]
  -> ModuleMap [IdeDeclarationAnn]
modifyAtDeclarationId declarationId f decls = do
  Map.update (Just . map update) (declarationId ^. ididModule) decls
  where
    update :: IdeDeclarationAnn -> IdeDeclarationAnn
    update d =
        if declarationMatchesId declarationId d
          then f d
          else d

declarationMatchesId :: IdeDeclarationId -> IdeDeclarationAnn -> Bool
declarationMatchesId declarationId d =
  namespaceForDeclaration (discardAnn d) == declarationId ^. ididNamespace
             && identifierFromIdeDeclaration (discardAnn d) == declarationId ^. ididIdentifier
