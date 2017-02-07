{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Command.Compile as Compile
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
            (Opts.info (Compile.command)
            (Opts.progDesc "Compile PureScript source files"))
        ]
