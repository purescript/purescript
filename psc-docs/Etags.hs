module Etags (dumpEtags) where

import qualified Language.PureScript as P
import Tags

dumpEtags :: [(String, P.Module)] -> [String]
dumpEtags = concat . (map renderModEtags)

renderModEtags :: (String, P.Module) -> [String]
renderModEtags (path, mdl) = ["\x0c", path ++ "," ++ show tagsLen] ++ tagLines
  where tagsLen = sum $ map length tagLines
        tagLines = map tagLine $ tags mdl
        tagLine (name, line) = "\x7f" ++ name ++ "\x01" ++ show line ++ ","


