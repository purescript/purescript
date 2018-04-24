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
  , ( C.PrimOrdering
    , mconcat [primOrderingTypes, primOrderingKinds]
    )
  , ( C.PrimRow
    , mconcat [primRowTypes, primRowClasses]
    )
  , ( C.PrimRowList
    , mconcat [primRowListTypes, primRowListClasses, primRowListKinds]
    )
  , ( C.PrimSymbol
    , mconcat [primSymbolTypes, primSymbolClasses]
    )
  , ( C.PrimTypeError
    , mconcat [primTypeErrorTypes, primTypeErrorClasses, primTypeErrorKinds]
    )
  ]
  where
    annType tys = foreach (Map.toList tys) $ \(tn, (kind, _)) ->
      IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType (P.disqualify tn) kind []))
    annClass cls = foreach (Map.toList cls) $ \(cn, _) ->
      -- Dummy kind and instances here, but we primarily care about the name completion
      IdeDeclarationAnn emptyAnn (IdeDeclTypeClass (IdeTypeClass (P.disqualify cn) P.kindType []) )

    primTypes = annType PEnv.primTypes
    primOrderingTypes = annType PEnv.primOrderingTypes
    primRowTypes = annType PEnv.primRowTypes
    primRowListTypes = annType PEnv.primRowListTypes
    primSymbolTypes = annType PEnv.primSymbolTypes
    primTypeErrorTypes = annType PEnv.primTypeErrorTypes

    primClasses = annClass PEnv.primClasses
    primRowClasses = annClass PEnv.primRowClasses
    primRowListClasses = annClass PEnv.primRowListClasses
    primSymbolClasses = annClass PEnv.primSymbolClasses
    primTypeErrorClasses = annClass PEnv.primTypeErrorClasses

    primKinds = foreach (Set.toList PEnv.primKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primOrderingKinds = foreach (Set.toList PEnv.primOrderingKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primRowListKinds = foreach (Set.toList PEnv.primRowListKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primTypeErrorKinds = foreach (Set.toList PEnv.primTypeErrorKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))
