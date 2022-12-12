{-# language PackageImports, BlockArguments #-}

module Language.PureScript.Ide.Externs
  ( readExternFile
  , convertExterns
  ) where

import           Protolude hiding (to, from, (&))

import           Codec.CBOR.Term as Term
import           Control.Lens hiding (anyOf)
import           "monad-logger" Control.Monad.Logger
import           Data.Version (showVersion)
import qualified Data.Text as Text
import qualified Language.PureScript as P
import qualified Language.PureScript.Make.Monad as Make
import           Language.PureScript.Ide.Error (IdeError (..))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util (properNameT)

readExternFile
  :: (MonadIO m, MonadError IdeError m, MonadLogger m)
  => FilePath
  -> m P.ExternsFile
readExternFile fp = do
  externsFile <- liftIO (Make.readCborFileIO fp)
  case externsFile of
    Just externs | version == P.efVersion externs ->
      pure externs
    _ ->
      liftIO (Make.readCborFileIO fp) >>= \case
        Just (Term.TList (_tag : Term.TString efVersion : _rest)) -> do
          let errMsg =
                "Version mismatch for the externs at: "
                <> toS fp
                <> " Expected: " <> version
                <> " Found: " <> efVersion
          logErrorN errMsg
          throwError (GeneralError errMsg)
        _ ->
          throwError (GeneralError ("Parsing the extern at: " <> toS fp <> " failed"))
    where
      version = toS (showVersion P.version)

convertExterns :: P.ExternsFile -> ([IdeDeclarationAnn], [(P.ModuleName, P.DeclarationRef)])
convertExterns ef =
  (decls, exportDecls)
  where
    decls = moduleDecl : map
      (IdeDeclarationAnn emptyAnn)
      (resolvedDeclarations <> operatorDecls <> tyOperatorDecls)
    exportDecls = mapMaybe convertExport (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    moduleDecl = IdeDeclarationAnn emptyAnn (IdeDeclModule (P.efModuleName ef))
    (toResolve, declarations) =
      second catMaybes (partitionEithers (map convertDecl (P.efDeclarations ef)))
    resolvedDeclarations = resolveSynonymsAndClasses toResolve declarations

resolveSynonymsAndClasses
  :: [ToResolve]
  -> [IdeDeclaration]
  -> [IdeDeclaration]
resolveSynonymsAndClasses trs decls = foldr go decls trs
  where
    go tr acc = case tr of
      TypeClassToResolve tcn ->
        case findType (P.coerceProperName tcn) acc of
          Nothing ->
            acc
          Just tyDecl -> IdeDeclTypeClass
            (IdeTypeClass tcn (tyDecl ^. ideTypeKind) [])
            : filter (not . anyOf (_IdeDeclType . ideTypeName) (== P.coerceProperName tcn)) acc
      SynonymToResolve tn ty ->
        case findType tn acc of
          Nothing ->
            acc
          Just tyDecl ->
            IdeDeclTypeSynonym (IdeTypeSynonym tn ty (tyDecl ^. ideTypeKind))
            : filter (not . anyOf (_IdeDeclType . ideTypeName) (== tn)) acc

findType :: P.ProperName 'P.TypeName -> [IdeDeclaration] -> Maybe IdeType
findType tn decls =
  decls
    & mapMaybe (preview _IdeDeclType)
    & find ((==) tn . view ideTypeName)

-- The Externs format splits information about synonyms across EDType
-- and EDTypeSynonym declarations. For type classes it split them
-- across an EDType and an EDClass . We collect these and resolve them
-- at the end of the conversion process.
data ToResolve
  = TypeClassToResolve (P.ProperName 'P.ClassName)
  | SynonymToResolve (P.ProperName 'P.TypeName) P.SourceType

convertExport :: P.DeclarationRef -> Maybe (P.ModuleName, P.DeclarationRef)
convertExport (P.ReExportRef _ src r) = Just (P.exportSourceDefinedIn src, r)
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Either ToResolve (Maybe IdeDeclaration)
convertDecl ed = case ed of
  -- We need to filter all types and synonyms that contain a '$'
  -- because those are typechecker internal definitions that shouldn't
  -- be user facing
  P.EDType{..} -> Right do
    guard (isNothing (Text.find (== '$') (edTypeName ^. properNameT)))
    Just (IdeDeclType (IdeType edTypeName edTypeKind []))
  P.EDTypeSynonym{..} ->
    if isNothing (Text.find (== '$') (edTypeSynonymName ^. properNameT))
      then Left (SynonymToResolve edTypeSynonymName edTypeSynonymType)
      else Right Nothing
  P.EDDataConstructor{..} -> Right do
    guard (isNothing (Text.find (== '$') (edDataCtorName ^. properNameT)))
    Just
      (IdeDeclDataConstructor
        (IdeDataConstructor edDataCtorName edDataCtorTypeCtor edDataCtorType))
  P.EDValue{..} ->
    Right (Just (IdeDeclValue (IdeValue edValueName edValueType)))
  P.EDClass{..} ->
    Left (TypeClassToResolve edClassName)
  P.EDInstance{} ->
    Right Nothing

convertOperator :: P.ExternsFixity -> IdeDeclaration
convertOperator P.ExternsFixity{..} =
  IdeDeclValueOperator
    (IdeValueOperator
      efOperator
      efAlias
      efPrecedence
      efAssociativity
      Nothing)

convertTypeOperator :: P.ExternsTypeFixity -> IdeDeclaration
convertTypeOperator P.ExternsTypeFixity{..} =
  IdeDeclTypeOperator
    (IdeTypeOperator
      efTypeOperator
      efTypeAlias
      efTypePrecedence
      efTypeAssociativity
      Nothing)
