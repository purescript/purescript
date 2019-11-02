
module Command.Docs (command, infoModList) where

import           Command.Docs.Html
import           Command.Docs.Markdown
import           Control.Applicative
import           Control.Monad.Writer
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import           Language.PureScript.Docs.Tags (dumpCtags, dumpEtags)
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeFile)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.FilePath.Glob (compile, glob, globDir1)
import           System.IO (hPutStrLn, stderr)
import           System.IO.UTF8 (writeUTF8FileT)

-- | Available output formats
data Format
  = Markdown
  | Html
  | Ctags -- Output ctags symbol index suitable for use with vi
  | Etags -- Output etags symbol index suitable for use with emacs
  deriving (Show, Eq, Ord)

data PSCDocsOptions = PSCDocsOptions
  { _pscdFormat :: Format
  , _pscdOutput :: Maybe FilePath
  , _pscdCompileOutputDir :: FilePath
  , _pscdInputFiles  :: [FilePath]
  }
  deriving (Show)

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt moutput compileOutput inputGlob) = do
  input <- concat <$> mapM glob inputGlob
  when (null input) $ do
    hPutStrLn stderr "purs docs: no input files."
    exitFailure

  let output = fromMaybe (defaultOutputForFormat fmt) moutput

  fileMs <- parseAndConvert input
  let ms = D.primModules ++ map snd fileMs
  case fmt of
    Etags -> writeTagsToFile output $ dumpEtags fileMs
    Ctags -> writeTagsToFile output $ dumpCtags fileMs
    Html -> do
      let ext = compile "*.html"
      let msHtml = map asHtml ms
      createDirectoryIfMissing True output
      globDir1 ext output >>= mapM_ removeFile
      writeHtmlModules output msHtml
    Markdown -> do
      let ext = compile "*.md"
      let msMarkdown = map asMarkdown ms
      createDirectoryIfMissing True output
      globDir1 ext output >>= mapM_ removeFile
      writeMarkdownModules output msMarkdown

  putStrLn $ "Documentation written to: " ++ output

  where
  successOrExit :: Either P.MultipleErrors a -> IO a
  successOrExit act =
    case act of
      Right x ->
        return x
      Left err -> do
        hPutStrLn stderr $ P.prettyPrintMultipleErrors P.defaultPPEOptions err
        exitFailure

  parseAndConvert input =
    runExceptT (fmap fst (D.collectDocs compileOutput input []))
    >>= successOrExit

  writeTagsToFile :: String -> [String] -> IO ()
  writeTagsToFile outputFilename tags = do
    currentDir <- getCurrentDirectory
    let outputFile = currentDir </> outputFilename
    let text = T.pack . unlines $ tags
    writeUTF8FileT outputFile text

instance Read Format where
  readsPrec _ "etags" = [(Etags, "")]
  readsPrec _ "ctags" = [(Ctags, "")]
  readsPrec _ "markdown" = [(Markdown, "")]
  readsPrec _ "html" = [(Html, "")]
  readsPrec _ _ = []

defaultOutputForFormat :: Format -> FilePath
defaultOutputForFormat fmt =
  case fmt of
    Markdown -> "generated-docs/md"
    Html -> "generated-docs/html"
    Etags -> "TAGS"
    Ctags -> "tags"

pscDocsOptions :: Opts.Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> format <*> output <*> compileOutputDir <*> many inputFile
  where
  format :: Opts.Parser Format
  format = Opts.option Opts.auto $
       Opts.value Html
    <> Opts.long "format"
    <> Opts.metavar "FORMAT"
    <> Opts.help "Set output FORMAT (markdown | html | etags | ctags)"

  output :: Opts.Parser (Maybe FilePath)
  output = optional $ Opts.strOption $
       Opts.long "output"
    <> Opts.short 'o'
    <> Opts.metavar "DEST"
    <> Opts.help "File/directory path for docs to be written to"

  compileOutputDir :: Opts.Parser FilePath
  compileOutputDir = Opts.strOption $
       Opts.value "output"
    <> Opts.showDefault
    <> Opts.long "compile-output"
    <> Opts.metavar "DIR"
    <> Opts.help "Compiler output directory"

  inputFile :: Opts.Parser FilePath
  inputFile = Opts.strArgument $
       Opts.metavar "FILE"
    <> Opts.help "The input .purs file(s)"

command :: Opts.Parser (IO ())
command = docgen <$> (Opts.helper <*> pscDocsOptions)

infoModList :: Opts.InfoMod a
infoModList = Opts.fullDesc <> footerInfo where
  footerInfo = Opts.footerDoc $ Just examples

examples :: PP.Doc
examples =
  PP.vcat $ map PP.text
    [ "Examples:"
    , "  write documentation for all modules to ./generated-docs:"
    , "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write documentation in Markdown format for all modules to ./generated-docs:"
    , "    purs docs --format markdown \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write CTags to ./tags:"
    , "    purs docs --format ctags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write ETags to ./TAGS:"
    , "    purs docs --format etags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    ]
