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

module Language.PureScript.Parser.JS
  ( ForeignJS()
  , parseForeignModulesFromFiles
  ) where

import Control.Applicative ((*>), (<*))
import Control.Monad (msum)
import Control.Monad.Error.Class (MonadError(..))
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer
import Prelude hiding (lex)
import qualified Data.Map as M
import qualified Text.Parsec as PS

type ForeignJS = String

parseForeignModulesFromFiles :: (MonadError MultipleErrors m, Functor m) => [(FilePath, ForeignJS)] -> m (M.Map ModuleName ForeignJS)
parseForeignModulesFromFiles files = do
  foreigns <- parU files $ \(path, file) ->
    case findModuleName (lines file) of
      Just name -> return (name, file)
      Nothing -> throwError (errorMessage $ ErrorParsingFFIModule path)
  return $ M.fromList foreigns

findModuleName :: [String] -> Maybe ModuleName
findModuleName = msum . map parseComment
  where
  parseComment :: String -> Maybe ModuleName
  parseComment s = either (const Nothing) Just $
    lex "" s >>= runTokenParser "" (symbol' "//" *> reserved "module" *> moduleName <* PS.eof)
