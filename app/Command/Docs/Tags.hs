module Command.Docs.Tags where

import           Control.Arrow (first)
import qualified Data.Text as T
import qualified Language.PureScript as P

tags :: P.Module -> [(String, Int)]
tags = map (first T.unpack) . concatMap dtags . P.exportedDeclarations
  where
    dtags :: P.Declaration -> [(P.Text, Int)]
    dtags (P.DataDeclaration (ss, _) _ name _ dcons) = (P.runProperName name, pos ss) : consNames
      where consNames = map (\(cname, _) -> (P.runProperName cname, pos ss)) dcons
    dtags (P.TypeDeclaration (P.TypeDeclarationData (ss, _) ident _)) = [(P.showIdent ident, pos ss)]
    dtags (P.ExternDeclaration (ss, _) ident _) = [(P.showIdent ident, pos ss)]
    dtags (P.TypeSynonymDeclaration (ss, _) name _ _) = [(P.runProperName name, pos ss)]
    dtags (P.TypeClassDeclaration (ss, _) name _ _ _ _) = [(P.runProperName name, pos ss)]
    dtags (P.TypeInstanceDeclaration (ss, _) _ _ name _ _ _ _) = [(P.showIdent name, pos ss)]
    dtags (P.ExternKindDeclaration (ss, _) name) = [(P.runProperName name, pos ss)]
    dtags _ = []
    pos :: P.SourceSpan -> Int
    pos = P.sourcePosLine . P.spanStart
