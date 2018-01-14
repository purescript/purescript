module Command.Docs.Ctags (dumpCtags) where

import           Command.Docs.Tags
import           Data.List (sort)
import           Language.PureScript.Docs.Types (Module)

dumpCtags :: [(String, Module)] -> [String]
dumpCtags = sort . concatMap renderModCtags

renderModCtags :: (String, Module) -> [String]
renderModCtags (path, mdl) = sort tagLines
  where tagLines = map tagLine $ tags mdl
        tagLine (name, line) = name ++ "\t" ++ path ++ "\t" ++ show line
