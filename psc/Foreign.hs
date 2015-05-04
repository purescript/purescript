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

module Foreign (parseForeignModulesFromFiles) where

import Control.Applicative ((*>), (<*))
import Control.Monad (msum)
import Control.Monad.Error.Class (MonadError(..))
import qualified Data.Map as M
import qualified Language.PureScript as P
import qualified Text.Parsec as PS

parseForeignModulesFromFiles :: (MonadError P.MultipleErrors m, Functor m) => [(FilePath, String)] -> m (M.Map P.ModuleName String)
parseForeignModulesFromFiles files = do
  foreigns <- P.parU files $ \(path, file) ->
    case findModuleName (lines file) of
      Just name -> return (name, file)
      Nothing -> throwError (P.errorMessage $ P.ErrorParsingFFIModule path)
  return $ M.fromList foreigns

findModuleName :: [String] -> Maybe P.ModuleName
findModuleName = msum . map parseComment
  where
  parseComment :: String -> Maybe P.ModuleName
  parseComment s = either (const Nothing) Just $
    P.lex "" s >>= P.runTokenParser "" (P.symbol' "//" *> P.reserved "module" *> P.moduleName <* PS.eof)
