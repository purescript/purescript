{-# LANGUAGE TupleSections #-}

module Command.Docs (command, infoModList) where

import           PSPrelude

import           GHC.Read (Read, readsPrec)
import           Command.Docs.Etags
import           Command.Docs.Ctags
import           Command.Docs.Html
import           Control.Category ((>>>))
import           Data.List (groupBy)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsMarkdown as D
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.FilePath.Glob (glob)

-- | Available output formats
data Format
  = Markdown
  | Html
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
  when (null input) $ fatal "purs docs: no input files."

  case fmt of
    Etags -> dumpTags input dumpEtags
    Ctags -> dumpTags input dumpCtags
    Html -> do
      let outputDir = "./generated-docs" -- TODO: make this configurable
      ms <- parseAndConvert input
      let msHtml = map asHtml (D.primDocsModule : ms)
      createDirectoryIfMissing False outputDir
      writeHtmlModules outputDir msHtml

    Markdown -> do
      ms <- parseAndConvert input

      case output of
        EverythingToStdOut ->
          putText (D.runDocs (D.modulesAsMarkdown ms))
        ToStdOut names -> do
          let (ms', missing) = takeByName ms names
          guardMissing missing
          putText (D.runDocs (D.modulesAsMarkdown ms'))
        ToFiles names -> do
          let (ms', missing) = takeByName' ms names
          guardMissing missing

          let ms'' = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ map swap ms'
          forM_ ms'' $ \grp -> do
            let fp = fst (unsafeHead grp)
            createDirectoryIfMissing True (takeDirectory fp)
            writeFile fp (D.runDocs (D.modulesAsMarkdown (map snd grp)))

  where
  guardMissing [] = return ()
  guardMissing [mn] = fatal ("purs docs: error: unknown module \"" <> P.runModuleName mn <> "\"")
  guardMissing mns = do
    putErrText "purs docs: error: unknown modules:"
    forM_ mns $ \mn ->
      putErrText $ "  * " <> P.runModuleName mn
    exitFailure

  successOrExit :: Either P.MultipleErrors a -> IO a
  successOrExit act =
    case act of
      Right x ->
        return x
      Left err -> fatal $ P.prettyPrintMultipleErrors P.defaultPPEOptions err

  takeByName = takeModulesByName D.modName
  takeByName' = takeModulesByName' D.modName

  parseAndConvert input =
    runExceptT (D.parseFilesInPackages input []
               >>= uncurry D.convertModulesInPackage)
    >>= successOrExit

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

dumpTags :: [FilePath] -> ([(FilePath, P.Module)] -> [Text]) -> IO ()
dumpTags input renderTags = do
  e <- P.parseModulesFromFiles (fromMaybe "") <$> mapM (fmap (first Just) . parseFile) (ordNub input)
  case e of
    Left err -> fatal $ show err
    Right ms ->
      ldump (renderTags (pairs ms))

  where
  pairs :: [(Maybe FilePath, m)] -> [(FilePath, m)]
  pairs = map (first (fromMaybe ""))

  ldump :: [Text] -> IO ()
  ldump = mapM_ putText

parseFile :: FilePath -> IO (FilePath, Text)
parseFile input = (,) input <$> readFile input

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)"

instance Read Format where
  readsPrec _ "etags" = [(Etags, "")]
  readsPrec _ "ctags" = [(Ctags, "")]
  readsPrec _ "markdown" = [(Markdown, "")]
  readsPrec _ "html" = [(Html, "")]
  readsPrec _ _ = []

format :: Opts.Parser Format
format = Opts.option Opts.auto $ Opts.value Markdown
         <> Opts.long "format"
         <> Opts.metavar "FORMAT"
         <> Opts.help "Set output FORMAT (markdown | html | etags | ctags)"

docgenModule :: Opts.Parser Text
docgenModule = Opts.strOption $
                   Opts.long "docgen"
                <> Opts.help "A list of module names which should appear in the output. This can optionally include file paths to write individual modules to, by separating with a colon ':'. For example, Prelude:docs/Prelude.md. This option may be specified multiple times."

pscDocsOptions :: Opts.Parser (Format, [FilePath], [Text])
pscDocsOptions = (,,) <$> format <*> many inputFile <*> many docgenModule

parseDocgen :: [Text] -> Either Text DocgenOutput
parseDocgen [] = Right EverythingToStdOut
parseDocgen xs = go xs
  where
  go = intersperse " "
    >>> T.concat
    >>> T.words
    >>> map parseItem
    >>> combine

data DocgenOutputItem
  = IToStdOut P.ModuleName
  | IToFile (P.ModuleName, FilePath)

parseItem :: Text -> DocgenOutputItem
parseItem s =
  case T.breakOn ":" s of
    (mn, "") -> IToStdOut $ P.moduleNameFromString mn
    (mn, fp) -> IToFile (P.moduleNameFromString mn, toS $ T.drop 1 fp)

combine :: [DocgenOutputItem] -> Either Text DocgenOutput
combine [] = Right EverythingToStdOut
combine (x:xs) = foldM go (initial x) xs
  where
  initial (IToStdOut m) = ToStdOut [m]
  initial (IToFile m)   = ToFiles [m]

  go (ToStdOut ms) (IToStdOut m) = Right (ToStdOut (m:ms))
  go (ToFiles ms) (IToFile m)    = Right (ToFiles (m:ms))
  go _ _ = Left "Can't mix module names and module name/file path pairs in the same invocation."

buildOptions :: (Format, [FilePath], [Text]) -> IO PSCDocsOptions
buildOptions (fmt, input, mapping) =
  case parseDocgen mapping of
    Right mapping' -> return (PSCDocsOptions fmt input mapping')
    Left err -> fatal $ "purs docs: error in --docgen option: " <> err

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
