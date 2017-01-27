-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Externs
-- Description : Handles externs files for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Handles externs files for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports  #-}

module Language.PureScript.Ide.Externs
  ( readExternFile
  , convertExterns
  , annotateModule
  ) where

import           Protolude

import           Control.Lens ((^.))
import           "monad-logger" Control.Monad.Logger
import           Data.Aeson (decodeStrict)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Version (showVersion)
import           Language.PureScript.Ide.Error (IdeError (..))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import qualified Language.PureScript as P

readExternFile :: (MonadIO m, MonadError IdeError m, MonadLogger m) =>
                  FilePath -> m P.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
   case parseResult of
     Nothing ->
       throwError (GeneralError
                   ("Parsing the extern at: " <> toS fp <> " failed"))
     Just externs
       | P.efVersion externs /= version -> do
           let errMsg = "Version mismatch for the externs at: " <> toS fp
                        <> " Expected: " <> version
                        <> " Found: " <> P.efVersion externs
           logErrorN errMsg
           throwError (GeneralError errMsg)
     Just externs -> pure externs

     where
       version = toS (showVersion P.version)

convertExterns :: P.ExternsFile -> ([IdeDeclarationAnn], [(P.ModuleName, P.DeclarationRef)])
convertExterns ef =
  (decls, exportDecls)
  where
    decls = map
      (IdeDeclarationAnn emptyAnn)
      (cleanDeclarations ++ operatorDecls ++ tyOperatorDecls)
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    declarations = mapMaybe convertDecl (P.efDeclarations ef)

    typeClassFilter = foldMap removeTypeDeclarationsForClass (filter isTypeClassDeclaration declarations)
    cleanDeclarations = ordNub (appEndo typeClassFilter declarations)

removeTypeDeclarationsForClass :: IdeDeclaration -> Endo [IdeDeclaration]
removeTypeDeclarationsForClass (IdeDeclTypeClass n) = Endo (filter notDuplicate)
  where notDuplicate (IdeDeclType t) =
          n ^. ideTCName . properNameT /= t ^. ideTypeName . properNameT
        notDuplicate (IdeDeclTypeSynonym s) =
          n ^. ideTCName . properNameT /= s ^. ideSynonymName . properNameT
        notDuplicate _ = True
removeTypeDeclarationsForClass _ = mempty

isTypeClassDeclaration :: IdeDeclaration -> Bool
isTypeClassDeclaration IdeDeclTypeClass{} = True
isTypeClassDeclaration _ = False

convertExport :: P.DeclarationRef -> Maybe (P.ModuleName, P.DeclarationRef)
convertExport (P.ReExportRef m r) = Just (m, r)
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Maybe IdeDeclaration
convertDecl P.EDType{..} = Just $ IdeDeclType $
  IdeType edTypeName edTypeKind
convertDecl P.EDTypeSynonym{..} = Just $ IdeDeclTypeSynonym
  (IdeTypeSynonym edTypeSynonymName edTypeSynonymType)
convertDecl P.EDDataConstructor{..} = Just $ IdeDeclDataConstructor $
  IdeDataConstructor edDataCtorName edDataCtorTypeCtor edDataCtorType
convertDecl P.EDValue{..} = Just $ IdeDeclValue $
  IdeValue edValueName edValueType
convertDecl P.EDClass{..} = Just $ IdeDeclTypeClass $
  IdeTypeClass edClassName []
convertDecl P.EDKind{..} = Just (IdeDeclKind edKindName)
convertDecl P.EDInstance{} = Nothing

convertOperator :: P.ExternsFixity -> IdeDeclaration
convertOperator P.ExternsFixity{..} =
  IdeDeclValueOperator $ IdeValueOperator
    efOperator
    efAlias
    efPrecedence
    efAssociativity
    Nothing

convertTypeOperator :: P.ExternsTypeFixity -> IdeDeclaration
convertTypeOperator P.ExternsTypeFixity{..} =
  IdeDeclTypeOperator $ IdeTypeOperator
    efTypeOperator
    efTypeAlias
    efTypePrecedence
    efTypeAssociativity
    Nothing

annotateModule
  :: (DefinitionSites P.SourceSpan, TypeAnnotations)
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
annotateModule (defs, types) decls =
  map convertDeclaration decls
  where
    convertDeclaration :: IdeDeclarationAnn -> IdeDeclarationAnn
    convertDeclaration (IdeDeclarationAnn ann d) = case d of
      IdeDeclValue v ->
        annotateFunction (v ^. ideValueIdent) (IdeDeclValue v)
      IdeDeclType t ->
        annotateType (t ^. ideTypeName . properNameT) (IdeDeclType t)
      IdeDeclTypeSynonym s ->
        annotateType (s ^. ideSynonymName . properNameT) (IdeDeclTypeSynonym s)
      IdeDeclDataConstructor dtor ->
        annotateValue (dtor ^. ideDtorName . properNameT) (IdeDeclDataConstructor dtor)
      IdeDeclTypeClass tc ->
        annotateType (tc ^. ideTCName . properNameT) (IdeDeclTypeClass tc)
      IdeDeclValueOperator op ->
        annotateValue (op ^. ideValueOpName . opNameT) (IdeDeclValueOperator op)
      IdeDeclTypeOperator op ->
        annotateType (op ^. ideTypeOpName . opNameT) (IdeDeclTypeOperator op)
      IdeDeclKind i ->
        annotateKind (i ^. properNameT) (IdeDeclKind i)
      where
        annotateFunction x = IdeDeclarationAnn (ann { annLocation = Map.lookup (IdeNSValue (P.runIdent x)) defs
                                                    , annTypeAnnotation = Map.lookup x types
                                                    })
        annotateValue x = IdeDeclarationAnn (ann {annLocation = Map.lookup (IdeNSValue x) defs})
        annotateType x = IdeDeclarationAnn (ann {annLocation = Map.lookup (IdeNSType x) defs})
        annotateKind x = IdeDeclarationAnn (ann {annLocation = Map.lookup (IdeNSKind x) defs})
