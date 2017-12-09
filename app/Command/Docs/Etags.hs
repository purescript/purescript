module Command.Docs.Etags (dumpEtags) where

import           PSPrelude

import           Command.Docs.Tags
import qualified Language.PureScript as P
import qualified Data.Text as T

dumpEtags :: [(FilePath, P.Module)] -> [Text]
dumpEtags = concatMap renderModEtags

renderModEtags :: (FilePath, P.Module) -> [Text]
renderModEtags (path, mdl) = ["\x0c", toS path <> "," <> show tagsLen] <> tagLines
  where tagsLen = sum $ map T.length tagLines
        tagLines = map tagLine $ tags mdl
        tagLine (name, line) = "\x7f" <> name <> "\x01" <> show line <> ","
