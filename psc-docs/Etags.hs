module Etags (dumpEtags) where

import qualified Language.PureScript as P
import Utils (taggables, getName, lineNumber)

dumpEtags :: [(String, P.Module)] -> [String]
dumpEtags ms = concat $ map renderModEtags ms

renderModEtags :: (String, P.Module) -> [String]
renderModEtags (path, mdl) = ["\x0c", path ++ "," ++ show tagsLen] ++ tags
  where tagsLen = foldl (\sofar l -> sofar+length l) 0 tags
        tags = map tagLine $ taggables mdl
        tagLine d = "\x7f" ++ getName d ++ "\x01" ++ show (lineNumber d) ++ ","


