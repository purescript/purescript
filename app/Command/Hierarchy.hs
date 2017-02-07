-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Hardy Jones 2014
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Hardy Jones <jones3.hardy@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Generate Directed Graphs of PureScript TypeClasses
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Command.Hierarchy (command) where

import           Control.Applicative (optional)
import           Control.Monad (unless)
import           Data.List (intercalate,nub,sort)
import           Data.Foldable (for_)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opts
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (hPutStr, stderr)
import           System.IO.UTF8 (readUTF8FileT)
import qualified Language.PureScript as P

data HierarchyOptions = HierarchyOptions
  { _hierachyInput   :: FilePath
  , _hierarchyOutput :: Maybe FilePath
  }

newtype SuperMap = SuperMap { _unSuperMap :: Either (P.ProperName 'P.ClassName) (P.ProperName 'P.ClassName, P.ProperName 'P.ClassName) }
  deriving Eq

instance Show SuperMap where
  show (SuperMap (Left sub)) = show sub
  show (SuperMap (Right (super, sub))) = show super ++ " -> " ++ show sub

instance Ord SuperMap where
  compare (SuperMap s) (SuperMap s') = getCls s `compare` getCls s'
    where
      getCls = either id snd

runModuleName :: P.ModuleName -> String
runModuleName (P.ModuleName pns) = intercalate "_" ((T.unpack . P.runProperName) `map` pns)

readInput :: [FilePath] -> IO (Either P.MultipleErrors [P.Module])
readInput paths = do
  content <- mapM (\path -> (path, ) <$> readUTF8FileT path) paths
  return $ map snd <$> P.parseModulesFromFiles id content

compile :: HierarchyOptions -> IO ()
compile (HierarchyOptions inputGlob mOutput) = do
  input <- glob inputGlob
  modules <- readInput input
  case modules of
    Left errs -> hPutStr stderr (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
    Right ms -> do
      for_ ms $ \(P.Module _ _ moduleName decls _) ->
        let name = runModuleName moduleName
            tcs = filter P.isTypeClassDeclaration decls
            supers = sort . nub . filter (not . null) $ fmap superClasses tcs
            prologue = "digraph " ++ name ++ " {\n"
            body = intercalate "\n" (concatMap (fmap (\s -> "  " ++ show s ++ ";")) supers)
            epilogue = "\n}"
            hier = prologue ++ body ++ epilogue
        in unless (null supers) $ case mOutput of
          Just output -> do
            createDirectoryIfMissing True output
            writeFile (output </> name) hier
          Nothing -> putStrLn hier
      exitSuccess

superClasses :: P.Declaration -> [SuperMap]
superClasses (P.TypeClassDeclaration sub _ supers@(_:_) _ _) =
  fmap (\(P.Constraint (P.Qualified _ super) _ _) -> SuperMap (Right (super, sub))) supers
superClasses (P.TypeClassDeclaration sub _ _ _ _) = [SuperMap (Left sub)]
superClasses (P.PositionedDeclaration _ _ decl) = superClasses decl
superClasses _ = []

inputFile :: Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.value "main.purs"
  <> Opts.showDefault
  <> Opts.help "The input file to generate a hierarchy from"

outputFile :: Parser (Maybe FilePath)
outputFile = optional . Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.help "The output directory"

pscOptions :: Parser HierarchyOptions
pscOptions = HierarchyOptions <$> inputFile
                              <*> outputFile

command :: Opts.Parser (IO ())
command = compile <$> (Opts.helper <*> pscOptions)
