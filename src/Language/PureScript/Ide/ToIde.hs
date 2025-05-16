module Language.PureScript.Ide.ToIde where

import Protolude hiding (moduleName, unzip)

import Control.Lens ((^.))
import Data.Map.Lazy qualified as Map
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Externs (ExternsFile(..))
import Language.PureScript.Ide.Externs (convertExterns)
import Language.PureScript.Ide.SourceFile (extractAstInformation)
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Util (opNameT, properNameT)
import Language.PureScript.AST.Declarations (Module (..))
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.Comments qualified as P

toIdeDeclarationAnn :: Module -> ExternsFile -> [IdeDeclarationAnn]
toIdeDeclarationAnn m e = results
  where
  asts = extractAstInformation m
  (moduleDeclarations, _) = convertExterns e
  results =
        moduleDeclarations
        -- & resolveDataConstructorsForModule
        & resolveLocationsForModule asts
        & resolveDocumentationForModule m
        -- & resolveInstances externs
        -- & resolveOperators
        -- & resolveReexports reexportRefs


resolveLocationsForModule
  :: (DefinitionSites P.SourceSpan, TypeAnnotations)
  -> [IdeDeclarationAnn]
  -> [IdeDeclarationAnn]
resolveLocationsForModule (defs, types) =
  map convertDeclaration
  where
    convertDeclaration :: IdeDeclarationAnn -> IdeDeclarationAnn
    convertDeclaration (IdeDeclarationAnn ann d) = convertDeclaration'
      annotateFunction
      annotateValue
      annotateDataConstructor
      annotateType
      annotateType -- type classes live in the type namespace
      annotateModule
      d
      where
        annotateFunction x = IdeDeclarationAnn (ann { _annLocation = Map.lookup (IdeNamespaced IdeNSValue (P.runIdent x)) defs
                                                    , _annTypeAnnotation = Map.lookup x types
                                                    })
        annotateValue x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
        annotateDataConstructor x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
        annotateType x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSType x) defs})
        annotateModule x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSModule x) defs})

convertDeclaration'
  :: (P.Ident -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
  -> IdeDeclaration
  -> IdeDeclarationAnn
convertDeclaration' annotateFunction annotateValue annotateDataConstructor annotateType annotateClass annotateModule d =
  case d of
    IdeDeclValue v ->
      annotateFunction (v ^. ideValueIdent) d
    IdeDeclType t ->
      annotateType (t ^. ideTypeName . properNameT) d
    IdeDeclTypeSynonym s ->
      annotateType (s ^. ideSynonymName . properNameT) d
    IdeDeclDataConstructor dtor ->
      annotateDataConstructor (dtor ^. ideDtorName . properNameT) d
    IdeDeclTypeClass tc ->
      annotateClass (tc ^. ideTCName . properNameT) d
    IdeDeclValueOperator operator ->
      annotateValue (operator ^. ideValueOpName . opNameT) d
    IdeDeclTypeOperator operator ->
      annotateType (operator ^. ideTypeOpName . opNameT) d
    IdeDeclModule mn ->
      annotateModule (P.runModuleName mn) d

resolveDocumentationForModule
  :: Module
    -> [IdeDeclarationAnn]
    -> [IdeDeclarationAnn]
resolveDocumentationForModule (Module _ moduleComments moduleName sdecls _) =
  map convertDecl
  where
  extractDeclComments :: P.Declaration -> [(P.Name, [P.Comment])]
  extractDeclComments = \case
    P.DataDeclaration (_, cs) _ ctorName _ ctors ->
      (P.TyName ctorName, cs) : map dtorComments ctors
    P.TypeClassDeclaration (_, cs) tyClassName _ _ _ members ->
      (P.TyClassName tyClassName, cs) : concatMap extractDeclComments members
    decl ->
      maybe [] (\name' -> [(name', snd (P.declSourceAnn decl))]) (name decl)

  comments :: Map.Map P.Name [P.Comment]
  comments = Map.insert (P.ModName moduleName) moduleComments $
    Map.fromListWith (flip (<>)) $ concatMap extractDeclComments sdecls

  dtorComments :: P.DataConstructorDeclaration -> (P.Name, [P.Comment])
  dtorComments dcd = (P.DctorName (P.dataCtorName dcd), snd (P.dataCtorAnn dcd))

  name :: P.Declaration -> Maybe P.Name
  name (P.TypeDeclaration d) = Just $ P.IdentName $ P.tydeclIdent d
  name decl = P.declName decl

  convertDecl :: IdeDeclarationAnn -> IdeDeclarationAnn
  convertDecl (IdeDeclarationAnn ann d) =
    convertDeclaration'
      (annotateValue . P.IdentName)
      (annotateValue . P.IdentName . P.Ident)
      (annotateValue . P.DctorName . P.ProperName)
      (annotateValue . P.TyName . P.ProperName)
      (annotateValue . P.TyClassName . P.ProperName)
      (annotateValue . P.ModName . P.moduleNameFromString)
      d
    where
      docs :: P.Name -> Text
      docs ident = fromMaybe "" $ convertComments =<< Map.lookup ident comments

      annotateValue ident = IdeDeclarationAnn (ann { _annDocumentation = Just $ docs ident })

-- resolveDataConstructorsForModule
--   :: [IdeDeclarationAnn]
--   -> [IdeDeclarationAnn]
-- resolveDataConstructorsForModule decls =
--   map (idaDeclaration %~ resolveDataConstructors) decls
--   where
--     resolveDataConstructors :: IdeDeclaration -> IdeDeclaration
--     resolveDataConstructors decl = case decl of
--       IdeDeclType ty ->
--         IdeDeclType (ty & ideTypeDtors .~ fromMaybe [] (Map.lookup (ty ^. ideTypeName) dtors))
--       _ ->
--         decl
--
--     dtors =
--       decls
--       & Map.mapMaybe (preview (idaDeclaration . _IdeDeclDataConstructor))
--       & foldr (\(IdeDataConstructor name typeName type') ->
--                   Map.insertWith (<>) typeName [(name, type')]) Map.empty
