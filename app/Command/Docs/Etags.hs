module Command.Docs.Etags (dumpEtags) where

import           Command.Docs.Tags
import qualified Language.PureScript as P

dumpEtags :: [(String, P.Module)] -> [String]
dumpEtags = concatMap renderModEtags

renderModEtags :: (String, P.Module) -> [String]
renderModEtags (path, mdl) = ["\x0c", path ++ "," ++ show tagsLen] ++ tagLines
  where tagsLen = sum $ map length tagLines
        tagLines = map tagLine $ tags mdl
        tagLine (name, line) = "\x7f" ++ name ++ "\x01" ++ show line ++ ","
