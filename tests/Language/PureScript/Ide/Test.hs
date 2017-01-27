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
import           System.Directory
import           System.FilePath
import           System.Process

import qualified Language.PureScript             as P

defConfig :: Configuration
defConfig =
  Configuration { confLogLevel = LogNone
                , confOutputPath = "output/"
                , confGlobs = ["src/*.purs"]
                }

runIde' :: Configuration -> IdeState -> [Command] -> IO ([Either IdeError Success], IdeState)
runIde' conf s cs = do
  stateVar <- newTVarIO s
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = conf}
  r <- runNoLoggingT (runReaderT (traverse (runExceptT . handleCommand) cs) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

runIde :: [Command] -> IO ([Either IdeError Success], IdeState)
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


ida :: IdeDeclaration -> IdeDeclarationAnn
ida = IdeDeclarationAnn emptyAnn

-- | Builders for Ide declarations
ideValue :: Text -> Maybe P.Type -> IdeDeclarationAnn
ideValue i ty = ida (IdeDeclValue (IdeValue (P.Ident i) (fromMaybe P.tyString ty)))

ideType :: Text -> Maybe P.Kind -> IdeDeclarationAnn
ideType pn ki = ida (IdeDeclType (IdeType (P.ProperName pn) (fromMaybe P.kindType ki)))

ideSynonym :: Text -> P.Type -> IdeDeclarationAnn
ideSynonym pn ty = ida (IdeDeclTypeSynonym (IdeTypeSynonym (P.ProperName pn) ty))

ideTypeClass :: Text -> [IdeInstance] -> IdeDeclarationAnn
ideTypeClass pn instances = ida (IdeDeclTypeClass (IdeTypeClass (P.ProperName pn) instances))

ideDtor :: Text -> Text -> Maybe P.Type -> IdeDeclarationAnn
ideDtor pn tn ty = ida (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName pn) (P.ProperName tn) (fromMaybe P.tyString ty)))

ideValueOp :: Text -> P.Qualified (Either Text Text) -> Integer -> Maybe P.Associativity -> Maybe P.Type -> IdeDeclarationAnn
ideValueOp opName ident precedence assoc t =
  ida (IdeDeclValueOperator
       (IdeValueOperator
        (P.OpName opName)
        (bimap P.Ident P.ProperName <$> ident)
        (precedence)
        (fromMaybe P.Infix assoc)
        t))

ideTypeOp :: Text -> P.Qualified Text -> Integer -> Maybe P.Associativity -> Maybe P.Kind -> IdeDeclarationAnn
ideTypeOp opName ident precedence assoc k =
  ida (IdeDeclTypeOperator
       (IdeTypeOperator
        (P.OpName opName)
        (P.ProperName <$> ident)
        (precedence)
        (fromMaybe P.Infix assoc)
        k))

ideKind :: Text -> IdeDeclarationAnn
ideKind pn = ida (IdeDeclKind (P.ProperName pn))

mn :: Text -> P.ModuleName
mn = P.moduleNameFromString

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("." </> "tests" </> "support" </> "pscide")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileTestProject :: IO Bool
compileTestProject = inProject $ do
  (_, _, _, procHandle) <-
    createProcess $ (shell $ "psc \"src/**/*.purs\"")
  r <- tryNTimes 10 (getProcessExitCode procHandle)
  pure (fromMaybe False (isSuccess <$> r))

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

tryNTimes :: Int -> IO (Maybe a) -> IO (Maybe a)
tryNTimes 0 _ = pure Nothing
tryNTimes n action = do
  r <- action
  case r of
    Nothing -> do
      threadDelay 500000
      tryNTimes (n - 1) action
    Just a -> pure (Just a)

deleteOutputFolder :: IO ()
deleteOutputFolder = inProject $
  whenM (doesDirectoryExist "output") (removeDirectoryRecursive "output")
