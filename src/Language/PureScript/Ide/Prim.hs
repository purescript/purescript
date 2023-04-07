module Language.PureScript.Ide.Prim (idePrimDeclarations) where

import Protolude

import Data.Text qualified as T
import Data.Map qualified as Map
import Language.PureScript qualified as P
import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.Environment qualified as PEnv
import Language.PureScript.Ide.Types (IdeDeclaration(..), IdeDeclarationAnn(..), IdeType(..), IdeTypeClass(..), ModuleMap, emptyAnn)

idePrimDeclarations :: ModuleMap [IdeDeclarationAnn]
idePrimDeclarations = Map.fromList
  [ ( C.M_Prim
    , mconcat [primTypes, primClasses]
    )
  , ( C.M_Prim_Boolean
    , mconcat [primBooleanTypes]
    )
  , ( C.M_Prim_Ordering
    , mconcat [primOrderingTypes]
    )
  , ( C.M_Prim_Row
    , mconcat [primRowTypes, primRowClasses]
    )
  , ( C.M_Prim_RowList
    , mconcat [primRowListTypes, primRowListClasses]
    )
  , ( C.M_Prim_Symbol
    , mconcat [primSymbolTypes, primSymbolClasses]
    )
  , ( C.M_Prim_Int
    , mconcat [primIntTypes, primIntClasses]
    )
  , ( C.M_Prim_TypeError
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
