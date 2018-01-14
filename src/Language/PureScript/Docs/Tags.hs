module Language.PureScript.Docs.Tags
  ( tags
  , dumpCtags
  , dumpEtags
  ) where

import Prelude

import           Control.Arrow (first)
import           Data.List (sort)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Language.PureScript.AST (SourceSpan, sourcePosLine, spanStart)
import Language.PureScript.Docs.Types

tags :: Module -> [(String, Int)]
tags = map (first T.unpack) . concatMap dtags . modDeclarations
  where
    dtags :: Declaration -> [(T.Text, Int)]
    dtags decl = case declSourceSpan decl of
      Just ss -> (declTitle decl, pos ss):(mapMaybe subtag $ declChildren decl)
      Nothing -> mapMaybe subtag $ declChildren decl

    subtag :: ChildDeclaration -> Maybe (T.Text, Int)
    subtag cdecl = case cdeclSourceSpan cdecl of
      Just ss -> Just (cdeclTitle cdecl, pos ss)
      Nothing -> Nothing

    pos :: SourceSpan -> Int
    pos = sourcePosLine . spanStart

-- etags files appear to be sorted on module file name:
-- from emacs source, `emacs/lib-src/etags.c`:
-- "In etags mode, sort by file name."
dumpEtags :: [(String, Module)] -> [String]
dumpEtags = concatMap renderModEtags . sort

renderModEtags :: (String, Module) -> [String]
renderModEtags (path, mdl) = ["\x0c", path ++ "," ++ show tagsLen] ++ tagLines
  where tagsLen = sum $ map length tagLines
        tagLines = map tagLine $ tags mdl
        tagLine (name, line) = "\x7f" ++ name ++ "\x01" ++ show line ++ ","

-- ctags files are required to be sorted: http://ctags.sourceforge.net/FORMAT
-- "The tags file is sorted on {tagname}.  This allows for a binary search in
--  the file."
dumpCtags :: [(String, Module)] -> [String]
dumpCtags = sort . concatMap renderModCtags

renderModCtags :: (String, Module) -> [String]
renderModCtags (path, mdl) = sort tagLines
  where tagLines = map tagLine $ tags mdl
        tagLine (name, line) = name ++ "\t" ++ path ++ "\t" ++ show line
