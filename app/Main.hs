module Main where

import Prelude

import qualified Command.Bundle as Bundle
import qualified Command.Codegen as Codegen
import qualified Command.Compile as Compile
import qualified Command.Docs as Docs
import qualified Command.Graph as Graph
import qualified Command.Hierarchy as Hierarchy
import qualified Command.Ide as Ide
import qualified Command.Publish as Publish
import qualified Command.REPL as REPL
import           Control.Monad (join)
import           Data.Foldable (fold)
import qualified Options.Applicative as Opts
import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Version (versionString)


main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering
    join $ Opts.handleParseResult . execParserPure opts =<< getArgs
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "The PureScript compiler and tools"
    footerInfo  = Opts.footerDoc (Just footer)

    footer =
      mconcat
        [ para $
            "For help using each individual command, run `purs COMMAND --help`. " ++
            "For example, `purs compile --help` displays options specific to the `compile` command."
        , Doc.hardline
        , Doc.hardline
        , Doc.text $ "purs " ++ versionString
        ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
        [ Opts.command "bundle"
            (Opts.info Bundle.command
              (Opts.progDesc "This command was removed in v0.15.0. Run this command for migration information."))
        , Opts.command "codegen"
            (Opts.info Codegen.command
              (Opts.progDesc "Generate JS from core functional representation"))
        , Opts.command "compile"
            (Opts.info Compile.command
              (Opts.progDesc "Compile PureScript source files"))
        , Opts.command "docs"
            (Opts.info Docs.command
              (Opts.progDesc "Generate documentation from PureScript source files in a variety of formats, including Markdown and HTML" <> Docs.infoModList))
        , Opts.command "graph"
            (Opts.info Graph.command
              (Opts.progDesc "Module dependency graph"))
        , Opts.command "hierarchy"
            (Opts.info Hierarchy.command
              (Opts.progDesc "Generate a GraphViz directed graph of PureScript type classes"))
        , Opts.command "ide"
            (Opts.info Ide.command
              (Opts.progDesc "Start or query an IDE server process"))
        , Opts.command "publish"
            (Opts.info Publish.command
              (Opts.progDesc "Generates documentation packages for upload to Pursuit"))
        , Opts.command "repl"
            (Opts.info REPL.command
              (Opts.progDesc "Enter the interactive mode (PSCi)"))
        ]
