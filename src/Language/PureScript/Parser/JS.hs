-----------------------------------------------------------------------------
--
-- Module      :  Foreign
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Parser.JS
  ( ForeignJS()
  , parseForeignModulesFromFiles
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((*>), (<*))
#endif
import Control.Monad (forM_, when, msum)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer
import Prelude hiding (lex)
import qualified Data.Map as M
import qualified Text.Parsec as PS

type ForeignJS = String

parseForeignModulesFromFiles :: (Functor m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
                             => [(FilePath, ForeignJS)]
                             -> m (M.Map ModuleName FilePath)
parseForeignModulesFromFiles files = do
  foreigns <- parU files $ \(path, file) ->
    case findModuleName (lines file) of
      Just name -> return (name, path)
      Nothing -> throwError (errorMessage $ ErrorParsingFFIModule path)
  let grouped = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) foreigns
  forM_ grouped $ \grp ->
    when (length grp > 1) $ do
      let mn = fst (head grp)
          paths = map snd grp
      tell $ errorMessage $ MultipleFFIModules mn paths
  return $ M.fromList foreigns

findModuleName :: [String] -> Maybe ModuleName
findModuleName = msum . map parseComment
  where
  parseComment :: String -> Maybe ModuleName
  parseComment s = either (const Nothing) Just $
    lex "" s >>= runTokenParser "" (symbol' "//" *> reserved "module" *> moduleName <* PS.eof)
