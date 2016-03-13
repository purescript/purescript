{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.PureScript.Ide.Externs
  (
    ExternDecl(..),
    ModuleIdent,
    DeclIdent,
    Type,
    Fixity(..),
    readExternFile,
    convertExterns,
    unwrapPositioned,
    unwrapPositionedRef
  ) where

import           Prelude                              ()
import           Prelude.Compat

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Maybe                           (mapMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.Externs          as PE
import           Language.PureScript.Ide.CodecJSON
import           Language.PureScript.Ide.Error        (PscIdeError (..))
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Names            as N
import qualified Language.PureScript.Pretty           as PP

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m PE.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeT <$> T.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " ++ fp ++ " failed"
     Just externs -> pure externs

moduleNameToText :: N.ModuleName -> Text
moduleNameToText = T.pack . N.runModuleName

properNameToText :: N.ProperName a -> Text
properNameToText = T.pack . N.runProperName

identToText :: N.Ident -> Text
identToText  = T.pack . N.runIdent

convertExterns :: PE.ExternsFile -> Module
convertExterns ef = (moduleName, exportDecls ++ importDecls ++ otherDecls)
  where
    moduleName = moduleNameToText (PE.efModuleName ef)
    importDecls = convertImport <$> PE.efImports ef
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (PE.efExports ef)
    -- Ignoring operator fixities for now since we're not using them
    -- operatorDecls = convertOperator <$> PE.efFixities ef
    otherDecls = mapMaybe convertDecl (PE.efDeclarations ef)

convertImport :: PE.ExternsImport -> ExternDecl
convertImport ei = Dependency
  (moduleNameToText (PE.eiModule ei))
  []
  (moduleNameToText <$> PE.eiImportedAs ei)

convertExport :: D.DeclarationRef -> Maybe ExternDecl
convertExport (D.ModuleRef mn) = Just (Export (moduleNameToText mn))
convertExport _ = Nothing

convertDecl :: PE.ExternsDeclaration -> Maybe ExternDecl
convertDecl PE.EDType{..} = Just $
  DataDecl
  (properNameToText edTypeName)
  (packAndStrip (PP.prettyPrintKind edTypeKind))
convertDecl PE.EDTypeSynonym{..} = Just $
  DataDecl
  (properNameToText edTypeSynonymName)
  (packAndStrip (PP.prettyPrintType edTypeSynonymType))
convertDecl PE.EDDataConstructor{..} = Just $
  DataDecl
  (properNameToText edDataCtorName)
  (packAndStrip (PP.prettyPrintType edDataCtorType))
convertDecl PE.EDValue{..} = Just $
  FunctionDecl
  (identToText edValueName)
  (packAndStrip (PP.prettyPrintType edValueType))
convertDecl _ = Nothing

packAndStrip :: String -> Text
packAndStrip = T.unwords . fmap T.strip . T.lines . T.pack

unwrapPositioned :: D.Declaration -> D.Declaration
unwrapPositioned (D.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x

unwrapPositionedRef :: D.DeclarationRef -> D.DeclarationRef
unwrapPositionedRef (D.PositionedDeclarationRef _ _ x) = x
unwrapPositionedRef x = x
