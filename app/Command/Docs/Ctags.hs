module Command.Docs.Ctags (dumpCtags) where

import           PSPrelude

import           Command.Docs.Tags
import           Data.List (sort)
import qualified Language.PureScript as P

dumpCtags :: [(FilePath, P.Module)] -> [Text]
dumpCtags = sort . concatMap renderModCtags

renderModCtags :: (FilePath, P.Module) -> [Text]
renderModCtags (path, mdl) = sort tagLines
  where tagLines = map tagLine $ tags mdl
        tagLine (name, line) = name <> "\t" <> toS path <> "\t" <> show line
