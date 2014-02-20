{-# LANGUAGE CPP #-}

module Main where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.Environment.XDG.BaseDir
#if !defined(mingw32_HOST_OS) || !defined(__MINGW32__)
import System.Posix.Files
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {postInst = setupXDG}

setupXDG :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
setupXDG _ _ _ _ = do
    configDir <- getUserConfigDir "purescript"
    createDirectoryIfMissing True configDir
#if !defined(mingw32_HOST_OS) || !defined(__MINGW32__)
    setFileMode configDir ownerModes
#endif
