module TestPrimDocs where

import Control.Monad
import Data.List ((\\))
import qualified Data.Map as Map
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsMarkdown as D

main :: IO ()
main = do
  putStrLn "Test that there are no bottoms hiding in primDocsModule"
  seq (D.runDocs (D.modulesAsMarkdown [D.primDocsModule])) (return ())

  putStrLn "Test that Prim is fully documented"
  let actualPrimTypes = map (P.runProperName . P.disqualify . fst) $ Map.toList P.primTypes
  let documentedPrimTypes = map D.declTitle (D.modDeclarations D.primDocsModule)

  let undocumentedTypes = actualPrimTypes \\ documentedPrimTypes
  let extraTypes = documentedPrimTypes \\ actualPrimTypes

  when (not (null undocumentedTypes)) $
    error $ "Undocumented Prim types: " ++ show undocumentedTypes

  when (not (null extraTypes)) $
    error $ "Extra Prim types: " ++ show undocumentedTypes
