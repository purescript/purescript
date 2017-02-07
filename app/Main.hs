{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Command.Bundle as Bundle
import qualified Command.Compile as Compile
import qualified Command.Docs as Docs
import qualified Command.Hierarchy as Hierarchy
import qualified Command.REPL as REPL
import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import           Data.Version (showVersion)
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import qualified System.IO as IO

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    cmd <- Opts.execParser opts
    cmd
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "The PureScript compiler and tools"
    footerInfo  = Opts.footer $ "psc " ++ showVersion Paths.version

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands = (Opts.subparser . fold)
        [ Opts.command "compile"
            (Opts.info Compile.command
            (Opts.progDesc "Compile PureScript source files"))
        , Opts.command "repl"
            (Opts.info REPL.command
            (Opts.progDesc "Enter the interactive mode (PSCi)"))
        , Opts.command "bundle"
            (Opts.info Bundle.command
            (Opts.progDesc "Bundle compiled PureScript modules for the browser"))
        , Opts.command "docs"
            (Opts.info Docs.command
            (Opts.progDesc "Generate Markdown documentation from PureScript source files" <> Docs.infoModList))
        , Opts.command "hierarchy"
            (Opts.info Hierarchy.command
            (Opts.progDesc "Generate a GraphViz directed graph of PureScript type classes"))
        ]
