module Language.PureScript.Ide.Prim (idePrimDeclarations) where

import           Protolude

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Language.PureScript as P
import qualified Language.PureScript.Constants.Prim as C
import qualified Language.PureScript.Environment as PEnv
import           Language.PureScript.Ide.Types

idePrimDeclarations :: ModuleMap [IdeDeclarationAnn]
idePrimDeclarations = Map.fromList
  [ ( C.Prim
    , mconcat [primTypes, primClasses]
    )
  , ( C.PrimBoolean
    , mconcat [primBooleanTypes]
    )
  , ( C.PrimOrdering
    , mconcat [primOrderingTypes]
    )
  , ( C.PrimRow
    , mconcat [primRowTypes, primRowClasses]
    )
  , ( C.PrimRowList
    , mconcat [primRowListTypes, primRowListClasses]
    )
  , ( C.PrimSymbol
    , mconcat [primSymbolTypes, primSymbolClasses]
    )
  , ( C.PrimInt
    , mconcat [primIntTypes, primIntClasses]
    )
  , ( C.PrimTypeError
    , mconcat [primTypeErrorTypes, primTypeErrorClasses]
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
    primIntTypes = annType (removeClasses PEnv.primIntTypes PEnv.primIntClasses)
    primTypeErrorTypes = annType (removeClasses PEnv.primTypeErrorTypes PEnv.primTypeErrorClasses)

    primClasses = annClass PEnv.primClasses
    primRowClasses = annClass PEnv.primRowClasses
    primRowListClasses = annClass PEnv.primRowListClasses
    primSymbolClasses = annClass PEnv.primSymbolClasses
    primIntClasses = annClass PEnv.primIntClasses
    primTypeErrorClasses = annClass PEnv.primTypeErrorClasses
