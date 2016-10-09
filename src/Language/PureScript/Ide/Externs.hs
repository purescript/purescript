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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.PureScript.Ide.Externs
  ( readExternFile
  , convertExterns
  , annotateModule
  ) where

import           Protolude

import           Control.Lens                  ((^.))
import           Data.Aeson                    (decodeStrict)
import qualified Data.ByteString               as BS
import           Data.List                     (nub)
import qualified Data.Map                      as Map
import           Language.PureScript.Ide.Error (PscIdeError (..))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import qualified Language.PureScript           as P

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m P.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " <> toS fp <> " failed"
     Just externs -> pure externs

convertExterns :: P.ExternsFile -> (Module, [(P.ModuleName, P.DeclarationRef)])
convertExterns ef =
  ((P.efModuleName ef, decls), exportDecls)
  where
    decls = map
      (IdeDeclarationAnn emptyAnn)
      (cleanDeclarations ++ operatorDecls ++ tyOperatorDecls)
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    declarations = mapMaybe convertDecl (P.efDeclarations ef)

    typeClassFilter = foldMap removeTypeDeclarationsForClass (filter isTypeClassDeclaration declarations)
    cleanDeclarations = nub $ appEndo typeClassFilter declarations

removeTypeDeclarationsForClass :: IdeDeclaration -> Endo [IdeDeclaration]
removeTypeDeclarationsForClass (IdeDeclTypeClass n) = Endo (filter notDuplicate)
  where notDuplicate (IdeDeclType t) = n ^. properNameT /= t ^. ideTypeName . properNameT
        notDuplicate (IdeDeclTypeSynonym s) = n ^. properNameT /= s ^. ideSynonymName . properNameT
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
  (IdeSynonym edTypeSynonymName edTypeSynonymType)
convertDecl P.EDDataConstructor{..} = Just $ IdeDeclDataConstructor $
  IdeDataConstructor edDataCtorName edDataCtorTypeCtor edDataCtorType
convertDecl P.EDValue{..} = Just $ IdeDeclValue $
  IdeValue edValueName edValueType
convertDecl P.EDClass{..} = Just (IdeDeclTypeClass edClassName)
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
  -> Module
  -> Module
annotateModule (defs, types) (moduleName, decls) =
  (moduleName, map convertDeclaration decls)
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
      IdeDeclTypeClass i ->
        annotateType (runProperNameT i) (IdeDeclTypeClass i)
      IdeDeclValueOperator op ->
        annotateValue (op ^. ideValueOpAlias & valueOperatorAliasT) (IdeDeclValueOperator op)
      IdeDeclTypeOperator op ->
        annotateType (op ^. ideTypeOpAlias & typeOperatorAliasT) (IdeDeclTypeOperator op)
      where
        annotateFunction x = IdeDeclarationAnn (ann { annLocation = Map.lookup (Left (runIdentT x)) defs
                                                    , annTypeAnnotation = Map.lookup x types
                                                    })
        annotateValue x = IdeDeclarationAnn (ann {annLocation = Map.lookup (Left x) defs})
        annotateType x = IdeDeclarationAnn (ann {annLocation = Map.lookup (Right x) defs})
