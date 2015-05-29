-----------------------------------------------------------------------------
--
-- Module      :  IO
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IO where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory
