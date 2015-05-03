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

module Foreign (parseForeignModulesFromFiles) where

import Control.Applicative ((*>), (<*))
import Control.Monad (forM, msum)
import qualified Data.Map as M
import qualified Language.PureScript as P
import qualified Text.Parsec as PS

parseForeignModulesFromFiles :: [(FilePath, String)] -> Either String (M.Map P.ModuleName String)
parseForeignModulesFromFiles files = do
  foreigns <- forM files $ \(path, file) -> do
    case findModuleName (lines file) of
      Just name -> Right (name, file)
      Nothing -> Left $ "Could not find a module definition comment in " ++ path
  return $ M.fromList foreigns

findModuleName :: [String] -> Maybe P.ModuleName
findModuleName = msum . map parseComment
  where
  parseComment :: String -> Maybe P.ModuleName
  parseComment s = either (const Nothing) Just $
    P.lex "" s >>= P.runTokenParser "" (P.symbol' "//" *> P.reserved "module" *> P.moduleName <* PS.eof)
