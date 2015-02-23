module Ctags (dumpCtags) where

import qualified Language.PureScript as P
import Tags
import Data.List (sort)

dumpCtags :: [(String, P.Module)] -> [String]
dumpCtags = sort . concat . (map renderModCtags)

renderModCtags :: (String, P.Module) -> [String]
renderModCtags (path, mdl) = sort tagLines
  where tagLines = map tagLine $ tags mdl
        tagLine (name, line) = name ++ "\t" ++ path ++ "\t" ++ show line
