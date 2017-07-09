module Language.PureScript.Ide.Prim (idePrimDeclarations) where

import           Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import qualified Language.PureScript.Environment as PEnv
import           Language.PureScript.Ide.Types

idePrimDeclarations :: [IdeDeclarationAnn]
idePrimDeclarations =
  primTypes <> primKinds <> primClasses
  where
    primTypes = foreach (Map.toList PEnv.primTypes) $ \(tn, (kind, _)) ->
      IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType (P.disqualify tn) kind []))
    primKinds = foreach (Set.toList PEnv.primKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))
    primClasses = foreach (Map.toList PEnv.primClasses) $ \(cn, _) ->
      -- Dummy kind and instances here, but we primarily care about the name completion
      IdeDeclarationAnn emptyAnn (IdeDeclTypeClass (IdeTypeClass (P.disqualify cn) P.kindType []) )
