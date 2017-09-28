module Language.PureScript.Ide.Completion
       ( getCompletions
       , getExactMatches
       , getExactCompletions
       , simpleExport
       , completionFromMatch
       , CompletionOptions(..)
       , defaultCompletionOptions
       , applyCompletionOptions
       ) where

import           Protolude hiding ((<&>), moduleName)

import           Control.Lens hiding ((&), op)
import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.PureScript as P
import           Language.PureScript.Ide.Error (prettyPrintTypeSingleLine)
import           Language.PureScript.Ide.Filter
import           Language.PureScript.Ide.Matcher
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

type Module = (P.ModuleName, [IdeDeclarationAnn])

-- | Applies the CompletionFilters and the Matcher to the given Modules
--   and sorts the found Completions according to the Matching Score
getCompletions
  :: [Filter]
  -> Matcher IdeDeclarationAnn
  -> CompletionOptions
  -> [Module]
  -> [Completion]
getCompletions filters matcher options modules =
  modules
  & applyFilters filters
  & matchesFromModules
  & runMatcher matcher
  & applyCompletionOptions options
  <&> completionFromMatch

getExactMatches :: Text -> [Filter] -> [Module] -> [Match IdeDeclarationAnn]
getExactMatches search filters modules =
  modules
  & applyFilters (equalityFilter search : filters)
  & matchesFromModules

getExactCompletions :: Text -> [Filter] -> [Module] -> [Completion]
getExactCompletions search filters modules =
  modules
  & getExactMatches search filters
  <&> simpleExport
  <&> completionFromMatch

matchesFromModules :: [Module] -> [Match IdeDeclarationAnn]
matchesFromModules = foldMap completionFromModule
  where
    completionFromModule (moduleName, decls) =
      map (\x -> Match (moduleName, x)) decls

data CompletionOptions = CompletionOptions
  { coMaxResults :: Maybe Int
  , coGroupReexports :: Bool
  }

instance FromJSON CompletionOptions where
  parseJSON = withObject "CompletionOptions" $ \o -> do
    maxResults <- o .:? "maxResults"
    groupReexports <- o .:? "groupReexports" .!= False
    pure (CompletionOptions { coMaxResults = maxResults
                            , coGroupReexports = groupReexports
                            })

defaultCompletionOptions :: CompletionOptions
defaultCompletionOptions = CompletionOptions { coMaxResults = Nothing, coGroupReexports = False }

applyCompletionOptions :: CompletionOptions -> [Match IdeDeclarationAnn] -> [(Match IdeDeclarationAnn, [P.ModuleName])]
applyCompletionOptions co decls =  decls
  & (if coGroupReexports co
      then groupCompletionReexports
      else map simpleExport)
  & maybe identity take (coMaxResults co)

simpleExport :: Match a -> (Match a, [P.ModuleName])
simpleExport match@(Match (moduleName, _)) = (match, [moduleName])

groupCompletionReexports :: [Match IdeDeclarationAnn] -> [(Match IdeDeclarationAnn, [P.ModuleName])]
groupCompletionReexports initial =
  Map.elems (foldr go Map.empty initial)
  where
    go (Match (moduleName, d@(IdeDeclarationAnn ann decl))) =
      let
        origin = fromMaybe moduleName (ann^.annExportedFrom)
      in
        Map.alter
        (insertDeclaration moduleName origin d)
        (Namespaced (namespaceForDeclaration decl)
         (P.runModuleName origin <> "." <> identifierFromIdeDeclaration decl))
    insertDeclaration moduleName origin d old = case old of
      Nothing -> Just ( Match (origin, d & idaAnnotation.annExportedFrom .~ Nothing)
                      , [moduleName]
                      )
      Just x -> Just (second (moduleName :) x)

data Namespaced a = Namespaced IdeNamespace a
  deriving (Show, Eq, Ord)

completionFromMatch :: (Match IdeDeclarationAnn, [P.ModuleName]) -> Completion
completionFromMatch (Match (m, IdeDeclarationAnn ann decl), mns) =
  Completion {..}
  where
    (complIdentifier, complExpandedType) = case decl of
      IdeDeclValue v -> (v ^. ideValueIdent . identT, v ^. ideValueType & prettyPrintTypeSingleLine)
      IdeDeclType t -> (t ^. ideTypeName . properNameT, t ^. ideTypeKind & P.prettyPrintKind)
      IdeDeclTypeSynonym s -> (s ^. ideSynonymName . properNameT, s ^. ideSynonymType & prettyPrintTypeSingleLine)
      IdeDeclDataConstructor d -> (d ^. ideDtorName . properNameT, d ^. ideDtorType & prettyPrintTypeSingleLine)
      IdeDeclTypeClass d -> (d ^. ideTCName . properNameT, d ^. ideTCKind & P.prettyPrintKind)
      IdeDeclValueOperator (IdeValueOperator op ref precedence associativity typeP) ->
        (P.runOpName op, maybe (showFixity precedence associativity (valueOperatorAliasT ref) op) prettyPrintTypeSingleLine typeP)
      IdeDeclTypeOperator (IdeTypeOperator op ref precedence associativity kind) ->
        (P.runOpName op, maybe (showFixity precedence associativity (typeOperatorAliasT ref) op) P.prettyPrintKind kind)
      IdeDeclKind k -> (P.runProperName k, "kind")

    complExportedFrom = mns

    complModule = P.runModuleName m

    complType = maybe complExpandedType prettyPrintTypeSingleLine (_annTypeAnnotation ann)

    complLocation = _annLocation ann

    complDocumentation = Nothing

    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, show p, r, "as", P.runOpName o]

