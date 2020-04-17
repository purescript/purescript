module Language.PureScript.Ide.Prim (idePrimDeclarations) where

import           Protolude

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.Environment as PEnv
import           Language.PureScript.Ide.Types

idePrimDeclarations :: ModuleMap [IdeDeclarationAnn]
idePrimDeclarations = Map.fromList
  [ ( C.Prim
    , mconcat [primTypes, primKinds, primClasses]
    )
  , ( C.PrimBoolean
    , mconcat [primBooleanTypes, primBooleanKinds]
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
    annType tys = flip mapMaybe (Map.toList tys) $ \(tn, (kind, _)) -> do
      let name = P.disqualify tn
      -- We need to remove the ClassName$Dict synonyms, because we
      -- don't want them to show up in completions
      guard (isNothing (T.find (== '$') (P.runProperName name)))
      Just (IdeDeclarationAnn emptyAnn (IdeDeclType (IdeType name kind [])))
    annClass cls = foreach (Map.toList cls) $ \(cn, _) ->
      -- Dummy kind and instances here, but we primarily care about the name completion
      IdeDeclarationAnn emptyAnn (IdeDeclTypeClass (IdeTypeClass (P.disqualify cn) P.kindType []) )
    -- The Environment for typechecking holds both a type class as well as a
    -- type declaration for every class, but we filter the types out when we
    -- load the Externs, so we do the same here
    removeClasses types classes =
      Map.difference types (Map.mapKeys (map P.coerceProperName) classes)

    primTypes = annType (removeClasses PEnv.primTypes PEnv.primClasses)
    primBooleanTypes = annType PEnv.primBooleanTypes
    primOrderingTypes = annType PEnv.primOrderingTypes
    primRowTypes = annType (removeClasses PEnv.primRowTypes PEnv.primRowClasses)
    primRowListTypes = annType (removeClasses PEnv.primRowListTypes PEnv.primRowListClasses)
    primSymbolTypes = annType (removeClasses PEnv.primSymbolTypes PEnv.primSymbolClasses)
    primTypeErrorTypes = annType (removeClasses PEnv.primTypeErrorTypes PEnv.primTypeErrorClasses)

    primClasses = annClass PEnv.primClasses
    primRowClasses = annClass PEnv.primRowClasses
    primRowListClasses = annClass PEnv.primRowListClasses
    primSymbolClasses = annClass PEnv.primSymbolClasses
    primTypeErrorClasses = annClass PEnv.primTypeErrorClasses

    primKinds = foreach (Set.toList PEnv.primKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primBooleanKinds = foreach (Set.toList PEnv.primBooleanKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primOrderingKinds = foreach (Set.toList PEnv.primOrderingKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primRowListKinds = foreach (Set.toList PEnv.primRowListKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))

    primTypeErrorKinds = foreach (Set.toList PEnv.primTypeErrorKinds) $ \kn ->
      IdeDeclarationAnn emptyAnn (IdeDeclKind (P.disqualify kn))
