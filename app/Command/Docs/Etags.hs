module Command.Docs.Etags (dumpEtags) where

import           Command.Docs.Tags
import           Language.PureScript.Docs.Types (Module)

dumpEtags :: [(String, Module)] -> [String]
dumpEtags = concatMap renderModEtags

renderModEtags :: (String, Module) -> [String]
renderModEtags (path, mdl) = ["\x0c", path ++ "," ++ show tagsLen] ++ tagLines
  where tagsLen = sum $ map length tagLines
        tagLines = map tagLine $ tags mdl
        tagLine (name, line) = "\x7f" ++ name ++ "\x01" ++ show line ++ ","
