module Command.Docs.Tags where

import           Control.Arrow (first)
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
