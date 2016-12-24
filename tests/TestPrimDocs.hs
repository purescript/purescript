module TestPrimDocs where

import Control.Monad
import Data.List ((\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsMarkdown as D

main :: IO ()
main = do
  putStrLn "Test that there are no bottoms hiding in primDocsModule"
  seq (D.runDocs (D.modulesAsMarkdown [D.primDocsModule])) (return ())

  putStrLn "Test that Prim is fully documented"
  let actualPrimNames =
        -- note that prim type classes are listed in P.primTypes
        (map (P.runProperName . P.disqualify . fst) $ Map.toList P.primTypes) ++
        (map (P.runProperName . P.disqualify) $ Set.toList P.primKinds)
  let documentedPrimNames = map D.declTitle (D.modDeclarations D.primDocsModule)

  let undocumentedNames = actualPrimNames \\ documentedPrimNames
  let extraNames = documentedPrimNames \\ actualPrimNames

  when (not (null undocumentedNames)) $
    error $ "Undocumented Prim names: " ++ show undocumentedNames

  when (not (null extraNames)) $
    error $ "Extra Prim names: " ++ show undocumentedNames
