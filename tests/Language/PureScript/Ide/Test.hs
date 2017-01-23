{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Language.PureScript.Ide.Test where

import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Map                        as Map
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Protolude

import qualified Language.PureScript             as P

defConfig :: Configuration
defConfig = Configuration {confLogLevel = LogNone , confOutputPath = "", confGlobs = []}

runIde' :: Configuration -> IdeState -> [Command] -> IO ([Either PscIdeError Success], IdeState)
runIde' conf s cs = do
  stateVar <- newTVarIO s
  let env = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = conf}
  r <- runNoLoggingT (runReaderT (traverse (runExceptT . handleCommand) cs) env)
  newState <- readTVarIO stateVar
  pure (r, newState)

runIde :: [Command] -> IO ([Either PscIdeError Success], IdeState)
runIde = runIde' defConfig emptyIdeState

s3 :: IdeState -> [(Text, [IdeDeclarationAnn])] -> IdeState
s3 s ds =
  s {ideStage3 = stage3}
  where
    stage3 = Stage3 (Map.fromList decls) Nothing
    decls = map (first P.moduleNameFromString) ds

-- | Adding Annotations to IdeDeclarations
ann :: IdeDeclarationAnn -> Annotation -> IdeDeclarationAnn
ann (IdeDeclarationAnn _ d) a = IdeDeclarationAnn a d

annLoc :: IdeDeclarationAnn -> P.SourceSpan -> IdeDeclarationAnn
annLoc (IdeDeclarationAnn a d) loc = IdeDeclarationAnn a {annLocation = Just loc} d

annExp :: IdeDeclarationAnn -> P.ModuleName -> IdeDeclarationAnn
annExp (IdeDeclarationAnn a d) e = IdeDeclarationAnn a {annExportedFrom = Just e} d

annTyp :: IdeDeclarationAnn -> P.Type -> IdeDeclarationAnn
annTyp (IdeDeclarationAnn a d) ta = IdeDeclarationAnn a {annTypeAnnotation = Just ta} d


-- | Builders for Ide declarations
ideValue :: Text -> Maybe P.Type -> IdeDeclarationAnn
ideValue i ty = IdeDeclarationAnn emptyAnn (IdeDeclValue (IdeValue (P.Ident i) (fromMaybe P.tyString ty)))

ideType :: Text -> Maybe P.Kind -> IdeDeclarationAnn
ideType pn ki = IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType (P.ProperName pn) (fromMaybe P.kindType ki)))

ideSynonym :: Text -> P.Type -> IdeDeclarationAnn
ideSynonym pn ty = IdeDeclarationAnn emptyAnn (IdeDeclSynonym (IdeSynonym (P.ProperName pn) ty))

ideTypeClass :: Text -> IdeDeclarationAnn
ideTypeClass pn = IdeDeclarationAnn emptyAnn (IdeDeclTypeClass (IdeTypeClass (P.ProperName pn) []))

ideDtor :: Text -> Text -> Maybe P.Type -> IdeDeclarationAnn
ideDtor pn tn ty = IdeDeclarationAnn emptyAnn (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName pn) (P.ProperName tn) (fromMaybe P.tyString ty)))

ideValueOp :: Text -> Either Text Text -> IdeDeclarationAnn
ideValueOp opName _ =
  IdeDeclarationAnn emptyAnn
    (IdeDeclValueOperator
     (IdeValueOperator
      (P.OpName opName)
      undefined
      undefined
      undefined
      undefined))

ideTypeOp :: Text -> Text -> IdeDeclarationAnn
ideTypeOp opName _ =
  IdeDeclarationAnn emptyAnn
    (IdeDeclTypeOperator
     (IdeTypeOperator
      (P.OpName opName)
      undefined
      undefined
      undefined
      undefined))

ideKind :: Text -> IdeDeclarationAnn
ideKind pn = IdeDeclarationAnn emptyAnn (IdeDeclKind (P.ProperName pn))
