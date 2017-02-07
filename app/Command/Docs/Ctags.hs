module Command.Docs.Ctags (dumpCtags) where

import           Command.Docs.Tags
import           Data.List (sort)
import qualified Language.PureScript as P

dumpCtags :: [(String, P.Module)] -> [String]
dumpCtags = sort . concatMap renderModCtags

renderModCtags :: (String, P.Module) -> [String]
renderModCtags (path, mdl) = sort tagLines
  where tagLines = map tagLine $ tags mdl
        tagLine (name, line) = name ++ "\t" ++ path ++ "\t" ++ show line
