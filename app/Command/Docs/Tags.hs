module Command.Docs.Tags where

import           Control.Arrow (first)
import qualified Data.Text as T
import qualified Language.PureScript as P

tags :: P.Module -> [(String, Int)]
tags = map (first T.unpack) . concatMap dtags . P.exportedDeclarations
  where dtags (P.PositionedDeclaration sp _ d) = map tag $ names d
          where tag name = (name, line)
                line = P.sourcePosLine $ P.spanStart sp
        dtags _ = []
        names (P.DataDeclaration _ name _ dcons) = P.runProperName name : consNames
          where consNames = map (\(cname, _) -> P.runProperName cname) dcons
        names (P.TypeDeclaration ident _) = [P.showIdent ident]
        names (P.ExternDeclaration ident _) = [P.showIdent ident]
        names (P.TypeSynonymDeclaration name _ _) = [P.runProperName name]
        names (P.TypeClassDeclaration name _ _ _ _) = [P.runProperName name]
        names (P.TypeInstanceDeclaration name _ _ _ _) = [P.showIdent name]
        names (P.ExternKindDeclaration name) = [P.runProperName name]
        names _ = []
