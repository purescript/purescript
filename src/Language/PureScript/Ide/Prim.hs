module Language.PureScript.Ide.Prim (idePrimDeclarations) where

import           Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Environment as PEnv
import           Language.PureScript.Ide.Types

idePrimDeclarations :: [(P.ModuleName, [IdeDeclarationAnn])]
idePrimDeclarations =
  [ ( C.Prim
    , mconcat [primTypes, primKinds, primClasses] 
    )
  , ( C.PrimRow
    , mconcat [primRowTypes, primRowClasses] 
    )
  , ( C.PrimTypeError
    , mconcat [primTypeErrorTypes, primTypeErrorClasses] 
    )
  ]
  where
    annType tys = foreach (Map.toList tys) $ \(tn, (kind, _)) ->
      IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType (P.disqualify tn) kind []))
    annClass cls = foreach (Map.toList cls) $ \(cn, _) ->
      -- Dummy kind and instances here, but we primarily care about the name completion
      IdeDeclarationAnn emptyAnn (IdeDeclTypeClass (IdeTypeClass (P.disqualify cn) P.kindType []) )

    primTypes = annType PEnv.primTypes
    primRowTypes = annType PEnv.primRowTypes
    primTypeErrorTypes = annType PEnv.primTypeErrorTypes

    primClasses = annClass PEnv.primClasses
    primRowClasses = annClass PEnv.primRowClasses
    primTypeErrorClasses = annClass PEnv.primTypeErrorClasses

    primKinds = foreach (Set.toList PEnv.primKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))
