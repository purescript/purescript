module Language.PureScript.Ide.Usages where

import           Protolude

import           Control.Lens ((^.))
import qualified Data.Text as T
import qualified Language.PureScript as P
import           Language.PureScript.Ide.Imports.Helpers
import           Language.PureScript.Ide.Types

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
  in
    flip mapMaybe freeNames $ \n@(ns, ident, ss) -> do
      resolvedModule <- resolveFreeName imports topLevelNames mn n
      pure $ Usage mn ss $ IdeDeclarationId resolvedModule ns $ P.runIdent (P.disqualify ident)

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
    <|> resolveAgainstSingleImplicitImport imports
  _ ->
    -- TODO(Christoph): Handle more cases
    Nothing

resolveAgainstSingleImplicitImport :: [Import] -> Maybe P.ModuleName
resolveAgainstSingleImplicitImport imports = case mapMaybe isImplicit imports of
  [implicit] -> Just implicit
  _ -> Nothing
  where
    isImplicit = \case
      Import mn P.Implicit Nothing -> Just mn
      Import mn (P.Hiding _) Nothing -> Just mn
      _ -> Nothing

resolveAgainstExplicitImport :: P.Ident -> Import -> Maybe P.ModuleName
resolveAgainstExplicitImport ident i = case i of
  Import mn (P.Explicit refs) Nothing ->
    guard (any refMatchesIdent refs) $> mn
  _ -> Nothing
  where
    refMatchesIdent = \case
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
        -- TODO(Christoph): Handle qualified identifiers
      _ -> []
