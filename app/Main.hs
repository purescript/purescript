{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified Command.Bundle as Bundle
import qualified Command.Compile as Compile
import qualified Command.Docs as Docs
import qualified Command.Hierarchy as Hierarchy
import qualified Command.Ide as Ide
import qualified Command.Publish as Publish
import qualified Command.REPL as REPL
import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import           Data.Version (showVersion)
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import qualified System.IO as IO

#ifndef RELEASE
import qualified Development.GitRev as GitRev
#endif

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
    footerInfo  = Opts.footer $ "purs " ++ showVersion Paths.version

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
        [ Opts.command "bundle"
            (Opts.info Bundle.command
              (Opts.progDesc "Bundle compiled PureScript modules for the browser"))
        , Opts.command "compile"
            (Opts.info Compile.command
              (Opts.progDesc "Compile PureScript source files"))
        , Opts.command "docs"
            (Opts.info Docs.command
              (Opts.progDesc "Generate Markdown documentation from PureScript source files" <> Docs.infoModList))
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

versionString :: String
versionString = showVersion Paths.version ++ extra
  where
#ifdef RELEASE
  extra = ""
#else
  extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
