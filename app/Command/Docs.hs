
module Command.Docs (command, infoModList) where

import           Command.Docs.Html
import           Command.Docs.Markdown
import           Control.Applicative
import           Control.Monad.Writer
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Functor ((<&>))
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import           Language.PureScript.Docs.Tags (dumpCtags, dumpEtags)
import qualified Options.Applicative as Opts
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.Exit (exitFailure)
import           System.FilePath.Glob (compile, glob, globDir)
import           System.IO (hPutStrLn, stderr)

-- | Available output formats
data Format
  = Markdown
  | Html
  | Ctags -- Output ctags symbol index suitable for use with vi
  | Etags -- Output etags symbol index suitable for use with emacs
  deriving (Show, Eq, Ord)

data PSCDocsOptions = PSCDocsOptions
  { _pscdFormat :: Format
  , _pscdInputFiles  :: [FilePath]
  }
  deriving (Show)

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt inputGlob) = do
  input <- concat <$> mapM glob inputGlob
  when (null input) $ do
    hPutStrLn stderr "purs docs: no input files."
    exitFailure

  fileMs <- parseAndConvert input
  let ms = map snd fileMs
  case fmt of
    Etags -> mapM_ putStrLn $ dumpEtags fileMs
    Ctags -> mapM_ putStrLn $ dumpCtags fileMs
    _ -> do
      let outputDir = "./generated-docs" -- TODO: make this configurable
      let ms' = D.primModules ++ ms
      let exts = map compile ["*.html", "*.md"]
      createDirectoryIfMissing False outputDir
      globDir exts outputDir <&> concat >>= mapM_ removeFile
      case fmt of
        Html -> do
          let msHtml = map asHtml ms'
          writeHtmlModules outputDir msHtml
        Markdown -> do
          let msMarkdown = map asMarkdown ms'
          writeMarkdownModules outputDir msMarkdown
        _ -> return ()

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
    runExceptT (D.parseFilesInPackages input []
                >>= uncurry D.convertTaggedModulesInPackage)
    >>= successOrExit

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

pscDocsOptions :: Opts.Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> format <*> many inputFile

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
    , "  write documentation in HTML format for all modules to ./generated-docs:"
    , "    purs docs --format html \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    ]
