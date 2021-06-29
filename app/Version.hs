{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Prelude

import Data.Version (showVersion)
import Paths_purescript as Paths

#ifndef RELEASE
import qualified Development.GitRev as GitRev
#endif

-- Unfortunately, Cabal doesn't support prerelease identifiers on versions. To
-- avoid misleading users who run `purs --version`, we manually add the
-- prerelease identifier here (if any). When releasing a proper version, simply
-- set this to an empty string.
prerelease :: String
prerelease = ""

versionString :: String
versionString = showVersion Paths.version ++ prerelease ++ extra
  where
#ifdef RELEASE
  extra = ""
#else
  extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
