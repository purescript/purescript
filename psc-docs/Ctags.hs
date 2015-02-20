module Ctags (dumpCtags) where

import qualified Language.PureScript as P
import Utils (taggables, getName, lineNumber)
import Data.List (sort)

dumpCtags :: [(String, P.Module)] -> [String]
dumpCtags ms = sort $ concat $ map renderModCtags ms

renderModCtags :: (String, P.Module) -> [String]
renderModCtags (path, mdl) = sort tags
  where tags = map tagLine $ taggables mdl
        tagLine d = getName d ++ "\t" ++ path ++ "\t" ++ show (lineNumber d)


