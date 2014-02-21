{-# LANGUAGE CPP #-}

#ifndef POSIX_LIKE
#define POSIX_LIKE !defined(_WIN32_HOST_OS) && \
                  (defined(unix_HOST_OS) || defined(__unix___HOST_OS) || \
                   defined(__unix_HOST_OS) || defined(linux_HOST_OS) || \
                   defined(__linux___HOST_OS) || defined(__linux_HOST_OS) || \
                   (defined(__APPLE___HOST_OS) && defined(__MACH___HOST_OS)))
#endif

module Main where

import Control.Monad

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

import System.Directory
import System.Environment.XDG.BaseDir
#if POSIX_LIKE
import System.Posix.Files
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {postInst = setupXDG}

setupXDG :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
setupXDG _ _ _ _ = do
    configDir <- getUserConfigDir "purescript"
    configExists <- doesDirectoryExist configDir
    putStrLn "\n\n\noh it worked!\n\n\n"
    unless configExists $ do
        createDirectoryIfMissing True configDir
#if POSIX_LIKE
        setFileMode configDir ownerModes
#endif
#undef POSIX_LIKE
