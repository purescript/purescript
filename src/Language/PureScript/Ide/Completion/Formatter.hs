module Language.PureScript.Ide.Completion.Formatter
  ( CompletionFormatter(..)
  , ReexportStrategy(..)
  , runFormatter
  , defaultFormatter
  ) where

import Protolude hiding ((&), from, to)
import Control.Lens hiding (op)
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Util
import qualified Language.PureScript as P

data ReexportStrategy
  = NoFlatten
  | Flatten
  | FlattenPrefer [P.ModuleName]

data TypeFormat
  = TypeFormat
    { tfsUnicode :: Bool
    , tfsMultiLine :: Bool
    }

data CompletionFormatter
  = CompletionFormatter ReexportStrategy
  | FullFormatter

defaultFormatter :: CompletionFormatter
defaultFormatter = CompletionFormatter NoFlatten

runFormatter :: CompletionFormatter -> [Match IdeDeclarationAnn] -> [Completion]
runFormatter (CompletionFormatter reexportStrategy) =
  let
    reexportHandler = case reexportStrategy of
      NoFlatten -> map (, Nothing)
      Flatten -> flattenReexports
      FlattenPrefer _ -> flattenReexports -- TODO: zething
  in
    map (completionFromMatch (TypeFormat True False)) . reexportHandler

completionFromMatch :: TypeFormat -> (Match IdeDeclarationAnn, Maybe [P.ModuleName]) -> Completion
completionFromMatch TypeFormat{..} (Match (m, IdeDeclarationAnn ann decl), reexports) =
  Completion {..}
  where
    ppType =
      (if tfsMultiLine then T.unwords . map T.strip . T.lines . T.pack else T.pack) .
      (if tfsUnicode then P.prettyPrintTypeWithUnicode else P.prettyPrintType)

    (complIdentifier, complExpandedType) = case decl of
      IdeDeclValue v -> (v ^. ideValueIdent . identT, v ^. ideValueType & ppType)
      IdeDeclType t -> (t ^. ideTypeName . properNameT, t ^. ideTypeKind & P.prettyPrintKind)
      IdeDeclTypeSynonym s -> (s ^. ideSynonymName . properNameT, s ^. ideSynonymType & ppType)
      IdeDeclDataConstructor d -> (d ^. ideDtorName . properNameT, d ^. ideDtorType & ppType)
      IdeDeclTypeClass d -> (d ^. ideTCName . properNameT, "type class")
      IdeDeclValueOperator (IdeValueOperator op ref precedence associativity typeP) ->
        (P.runOpName op, maybe (showFixity precedence associativity (valueOperatorAliasT ref) op) ppType typeP)
      IdeDeclTypeOperator (IdeTypeOperator op ref precedence associativity kind) ->
        (P.runOpName op, maybe (showFixity precedence associativity (typeOperatorAliasT ref) op) P.prettyPrintKind kind)
      IdeDeclKind k -> (P.runProperName k, "kind")

    complModule = P.runModuleName m

    complReexports = map (map P.runModuleName) reexports

    complType = maybe complExpandedType ppType (_annTypeAnnotation ann)

    complLocation = _annLocation ann

    complDocumentation = Nothing

    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, show p, r, "as", P.runOpName o]

flattenReexports :: [Match IdeDeclarationAnn] -> [(Match IdeDeclarationAnn, Maybe [P.ModuleName])]
flattenReexports initial = second Just <$> Map.elems (foldr go Map.empty initial)
  where
    go (Match (moduleName, d@(IdeDeclarationAnn (_annExportedFrom -> Just origin) decl))) =
      Map.alter
      (insertReexport moduleName origin d)
      (getNS decl (P.runModuleName origin <> "." <> identifierFromIdeDeclaration decl))
    go (Match (moduleName, d@(IdeDeclarationAnn _ decl))) =
      Map.alter
      (insertDeclaration moduleName d)
      (getNS decl (P.runModuleName moduleName <> "." <> identifierFromIdeDeclaration decl))
    insertReexport moduleName origin d old = case old of
      Nothing -> Just (Match (origin, d), [moduleName])
      Just x -> Just (second (moduleName :) x)
    insertDeclaration moduleName d old = case old of
      Nothing -> Just (Match (moduleName, d), [])
      Just x -> Just x


getNS :: IdeDeclaration -> Text -> IdeDeclNamespace
getNS d = case d of
  IdeDeclValue _ -> IdeNSValue
  IdeDeclType _ -> IdeNSType
  IdeDeclTypeSynonym _ -> IdeNSType
  IdeDeclDataConstructor _ -> IdeNSValue
  IdeDeclTypeClass _ -> IdeNSType
  IdeDeclValueOperator _ -> IdeNSValue
  IdeDeclTypeOperator _ -> IdeNSType
  IdeDeclKind _ -> IdeNSKind

instance FromJSON ReexportStrategy where
  parseJSON = withObject "ReexportStrategy" $ \o -> do
    strategy :: Text <- o .: "strategy"
    case strategy of
      "noflatten" -> pure NoFlatten
      "flatten" -> pure Flatten
      "flattenPrefer" -> FlattenPrefer <$> o .: "modules"
      _ -> mzero

instance FromJSON CompletionFormatter where
  parseJSON = withObject "CompletionFormatter" $ \o -> do
    formatterType :: Text <- o .: "formatter"
    case formatterType of
      "completion" -> do
        params <- o .:? "params"
        case params of
          Just p ->
            CompletionFormatter <$> p .: "reexportStrategy"
          Nothing ->
            pure (CompletionFormatter NoFlatten)
      "full" -> do
        pure FullFormatter
      _ -> mzero

