module Tags where

import qualified Language.PureScript as P

tags :: P.Module -> [(String, Int)]
tags = concatMap dtags . P.exportedDeclarations
  where dtags (P.PositionedDeclaration sp _ d) = map tag $ names d
          where tag name = (name, line)
                line = P.sourcePosLine $ P.spanStart sp
        dtags _ = []
        names (P.DataDeclaration _ name _ dcons) = P.runProperName name : consNames
          where consNames = map (\(cname, _) -> P.runProperName cname) dcons
        names (P.TypeDeclaration ident _) = [show ident]
        names (P.ExternDeclaration _ ident _ _) = [show ident]
        names (P.TypeSynonymDeclaration name _ _) = [P.runProperName name]
        names (P.TypeClassDeclaration name _ _ _) = [P.runProperName name]
        names (P.TypeInstanceDeclaration name _ _ _ _) = [show name]
        names _ = []
