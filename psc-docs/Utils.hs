module Utils where

import qualified Language.PureScript as P

getName :: P.Declaration -> String
getName (P.TypeDeclaration ident _) = show ident
getName (P.ExternDeclaration _ ident _ _) = show ident
getName (P.DataDeclaration _ name _ _) = P.runProperName name
getName (P.ExternDataDeclaration name _) = P.runProperName name
getName (P.TypeSynonymDeclaration name _ _) = P.runProperName name
getName (P.TypeClassDeclaration name _ _ _) = P.runProperName name
getName (P.TypeInstanceDeclaration name _ _ _ _) = show name
getName (P.PositionedDeclaration _ _ d) = getName d
getName _ = error "Invalid argument to getName"

isValueDeclaration :: P.Declaration -> Bool
isValueDeclaration P.TypeDeclaration{} = True
isValueDeclaration P.ExternDeclaration{} = True
isValueDeclaration (P.PositionedDeclaration _ _ d) = isValueDeclaration d
isValueDeclaration _ = False

isTypeDeclaration :: P.Declaration -> Bool
isTypeDeclaration P.DataDeclaration{} = True
isTypeDeclaration P.ExternDataDeclaration{} = True
isTypeDeclaration P.TypeSynonymDeclaration{} = True
isTypeDeclaration (P.PositionedDeclaration _ _ d) = isTypeDeclaration d
isTypeDeclaration _ = False

isTypeClassDeclaration :: P.Declaration -> Bool
isTypeClassDeclaration P.TypeClassDeclaration{} = True
isTypeClassDeclaration (P.PositionedDeclaration _ _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

isTypeInstanceDeclaration :: P.Declaration -> Bool
isTypeInstanceDeclaration P.TypeInstanceDeclaration{} = True
isTypeInstanceDeclaration (P.PositionedDeclaration _ _ d) = isTypeInstanceDeclaration d
isTypeInstanceDeclaration _ = False

lineNumber :: P.Declaration -> Int
lineNumber (P.PositionedDeclaration sp _ _) = P.sourcePosLine $ P.spanStart sp
lineNumber _ = error "Not a PositionedDeclaration"

taggables :: P.Module -> [P.Declaration]
taggables = (filter taggable) . (filter isPositionedDeclaration) . P.exportedDeclarations
  where isPositionedDeclaration (P.PositionedDeclaration _ _ _) = True
        isPositionedDeclaration _ = False
        taggable :: P.Declaration -> Bool
        taggable d = isTypeDeclaration d
             || isTypeClassDeclaration d
             || isTypeInstanceDeclaration d
             || isValueDeclaration d

