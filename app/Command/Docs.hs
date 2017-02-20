{-# LANGUAGE TupleSections #-}

module Command.Docs (command, infoModList) where

import           Protolude (ordNub)

import           Command.Docs.Etags
import           Command.Docs.Ctags
import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Category ((>>>))
import           Control.Monad.Writer
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Function (on)
import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Tuple (swap)
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsMarkdown as D
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (exitFailure)
import           System.FilePath (takeDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStrLn, hPrint, stderr)
import           System.IO.UTF8 (readUTF8FileT, writeUTF8FileT)

-- Available output formats
data Format = Markdown -- Output documentation in Markdown format
               | Ctags -- Output ctags symbol index suitable for use with vi
               | Etags -- Output etags symbol index suitable for use with emacs
               deriving (Show, Eq, Ord)

-- | Available methods of outputting Markdown documentation
data DocgenOutput
  = EverythingToStdOut
  | ToStdOut [P.ModuleName]
  | ToFiles [(P.ModuleName, FilePath)]
  deriving (Show)

data PSCDocsOptions = PSCDocsOptions
  { _pscdFormat :: Format
  , _pscdInputFiles  :: [FilePath]
  , _pscdDocgen :: DocgenOutput
  }
  deriving (Show)

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt inputGlob output) = do
  input <- concat <$> mapM glob inputGlob
  case fmt of
    Etags -> dumpTags input dumpEtags
    Ctags -> dumpTags input dumpCtags
    Markdown -> do
      ms <- runExceptT (D.parseFilesInPackages input []
                           >>= uncurry D.convertModulesInPackage)
               >>= successOrExit

      case output of
        EverythingToStdOut ->
          T.putStrLn (D.runDocs (D.modulesAsMarkdown ms))
        ToStdOut names -> do
          let (ms', missing) = takeByName ms names
          guardMissing missing
          T.putStrLn (D.runDocs (D.modulesAsMarkdown ms'))
        ToFiles names -> do
          let (ms', missing) = takeByName' ms names
          guardMissing missing

          let ms'' = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ map swap ms'
          forM_ ms'' $ \grp -> do
            let fp = fst (head grp)
            createDirectoryIfMissing True (takeDirectory fp)
            writeUTF8FileT fp (D.runDocs (D.modulesAsMarkdown (map snd grp)))

  where
  guardMissing [] = return ()
  guardMissing [mn] = do
    hPutStrLn stderr ("purs docs: error: unknown module \"" ++ T.unpack (P.runModuleName mn) ++ "\"")
    exitFailure
  guardMissing mns = do
    hPutStrLn stderr "purs docs: error: unknown modules:"
    forM_ mns $ \mn ->
      hPutStrLn stderr ("  * " ++ T.unpack (P.runModuleName mn))
    exitFailure

  successOrExit :: Either P.MultipleErrors a -> IO a
  successOrExit act =
    case act of
      Right x ->
        return x
      Left err -> do
        hPutStrLn stderr $ P.prettyPrintMultipleErrors P.defaultPPEOptions err
        exitFailure

  takeByName = takeModulesByName D.modName
  takeByName' = takeModulesByName' D.modName

-- |
-- Given a list of module names and a list of modules, return a list of modules
-- whose names appeared in the given name list, together with a list of names
-- for which no module could be found in the module list.
--
takeModulesByName :: (Eq n) => (m -> n) -> [m] -> [n] -> ([m], [n])
takeModulesByName getModuleName modules names =
  first (map fst) (takeModulesByName' getModuleName modules (map (,()) names))

-- |
-- Like takeModulesByName, but also keeps some extra information with each
-- module.
--
takeModulesByName' :: (Eq n) => (m -> n) -> [m] -> [(n, a)] -> ([(m, a)], [n])
takeModulesByName' getModuleName modules = foldl go ([], [])
  where
  go (ms, missing) (name, x) =
    case find ((== name) . getModuleName) modules of
      Just m  -> ((m, x) : ms, missing)
      Nothing -> (ms, name : missing)

dumpTags :: [FilePath] -> ([(String, P.Module)] -> [String]) -> IO ()
dumpTags input renderTags = do
  e <- P.parseModulesFromFiles (fromMaybe "") <$> mapM (fmap (first Just) . parseFile) (ordNub input)
  case e of
    Left err -> do
      hPrint stderr err
      exitFailure
    Right ms ->
      ldump (renderTags (pairs ms))

  where
  pairs :: [(Maybe String, m)] -> [(String, m)]
  pairs = map (first (fromMaybe ""))

  ldump :: [String] -> IO ()
  ldump = mapM_ putStrLn

parseFile :: FilePath -> IO (FilePath, Text)
parseFile input = (,) input <$> readUTF8FileT input

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)"

instance Read Format where
  readsPrec _ "etags" = [(Etags, "")]
  readsPrec _ "ctags" = [(Ctags, "")]
  readsPrec _ "markdown" = [(Markdown, "")]
  readsPrec _ _ = []

format :: Opts.Parser Format
format = Opts.option Opts.auto $ Opts.value Markdown
         <> Opts.long "format"
         <> Opts.metavar "FORMAT"
         <> Opts.help "Set output FORMAT (markdown | etags | ctags)"

docgenModule :: Opts.Parser String
docgenModule = Opts.strOption $
                   Opts.long "docgen"
                <> Opts.help "A list of module names which should appear in the output. This can optionally include file paths to write individual modules to, by separating with a colon ':'. For example, Prelude:docs/Prelude.md. This option may be specified multiple times."

pscDocsOptions :: Opts.Parser (Format, [FilePath], [String])
pscDocsOptions = (,,) <$> format <*> many inputFile <*> many docgenModule

parseDocgen :: [String] -> Either String DocgenOutput
parseDocgen [] = Right EverythingToStdOut
parseDocgen xs = go xs
  where
  go = intersperse " "
    >>> concat
    >>> words
    >>> map parseItem
    >>> combine

data DocgenOutputItem
  = IToStdOut P.ModuleName
  | IToFile (P.ModuleName, FilePath)

parseItem :: String -> DocgenOutputItem
parseItem s = case elemIndex ':' s of
  Just i ->
    s # splitAt i
        >>> first (P.moduleNameFromString . T.pack)
        >>> second (drop 1)
        >>> IToFile
  Nothing ->
    IToStdOut (P.moduleNameFromString (T.pack s))

  where
  infixr 1 #
  (#) = flip ($)

combine :: [DocgenOutputItem] -> Either String DocgenOutput
combine [] = Right EverythingToStdOut
combine (x:xs) = foldM go (initial x) xs
  where
  initial (IToStdOut m) = ToStdOut [m]
  initial (IToFile m)   = ToFiles [m]

  go (ToStdOut ms) (IToStdOut m) = Right (ToStdOut (m:ms))
  go (ToFiles ms) (IToFile m)    = Right (ToFiles (m:ms))
  go _ _ = Left "Can't mix module names and module name/file path pairs in the same invocation."

buildOptions :: (Format, [FilePath], [String]) -> IO PSCDocsOptions
buildOptions (fmt, input, mapping) =
  case parseDocgen mapping of
    Right mapping' -> return (PSCDocsOptions fmt input mapping')
    Left err -> do
      hPutStrLn stderr "purs docs: error in --docgen option:"
      hPutStrLn stderr ("  " ++ err)
      exitFailure

command :: Opts.Parser (IO ())
command = (buildOptions >=> docgen) <$> (Opts.helper <*> pscDocsOptions)

infoModList :: Opts.InfoMod a
infoModList = Opts.fullDesc <> footerInfo where
  footerInfo = Opts.footerDoc $ Just examples

examples :: PP.Doc
examples =
  PP.vcat $ map PP.text
    [ "Examples:"
    , "  print documentation for Data.List to stdout:"
    , "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\" \\"
    , "      --docgen Data.List"
    , ""
    , "  write documentation for Data.List to docs/Data.List.md:"
    , "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\" \\"
    , "      --docgen Data.List:docs/Data.List.md"
    , ""
    , "  write documentation for Data.List to docs/Data.List.md, and"
    , "  documentation for Data.List.Lazy to docs/Data.List.Lazy.md:"
    , "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\" \\"
    , "      --docgen Data.List:docs/Data.List.md \\"
    , "      --docgen Data.List.Lazy:docs/Data.List.Lazy.md"
    ]
