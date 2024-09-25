-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.State
-- Description : Functions to access psc-ide's state
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Functions to access psc-ide's state
-----------------------------------------------------------------------------

{-# LANGUAGE TypeApplications #-}

module Language.PureScript.Ide.ToI where

import Protolude hiding (moduleName, unzip)

import Control.Concurrent.STM (TVar, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens (Ixed(..), preview, view, (%~), (.~), (^.))
import Data.IORef (readIORef, writeIORef)
import Data.Map.Lazy qualified as Map
import Data.Time.Clock (UTCTime)
import Data.Zip (unzip)
import Language.PureScript.Docs.Convert.Single (convertComments)
import Language.PureScript.Externs (ExternsDeclaration(..), ExternsFile(..))
import Language.PureScript.Ide.Externs (convertExterns)
import Language.PureScript.Ide.SourceFile (extractAstInformation)
import Language.PureScript.Ide.Types
import Language.PureScript.Ide.Util (discardAnn, displayTimeSpec, logPerf, opNameT, properNameT, runLogger)
import System.Directory (getModificationTime)
import Database.SQLite.Simple qualified as SQLite
import Debug.Trace qualified as Debug
import Language.PureScript.AST.Declarations qualified as P
import Language.PureScript.AST.SourcePos qualified as P
import Language.PureScript.Names qualified as P
import Language.PureScript.Comments qualified as P
import Language.PureScript.Externs qualified as P
import Language.PureScript.Ide.Reexports (resolveReexports)


-- toI :: P.Module -> ExternsFile -> [IdeDeclarationAnn]
-- toI m e = do
--   let externs = Map.singleton (P.getModuleName m) e
--   let modules = Map.singleton (P.getModuleName m) (m, "adfasd")
--   let asts = map (extractAstInformation . fst) modules
--   let (moduleDeclarations, reexportRefs) = unzip (Map.map convertExterns externs)
--       results =
--         moduleDeclarations
--         & map resolveDataConstructorsForModule
--         & resolveLocations asts
--         & resolveDocumentation (map fst modules)
--         & resolveInstances externs
--         & resolveOperators
--         & resolveReexports reexportRefs
--   fromMaybe [] $ Map.lookup (P.getModuleName m) (map reResolved results)
--
-- toIdeDeclarationAnn :: P.Module -> ExternsFile -> [IdeDeclarationAnn]
-- toIdeDeclarationAnn m e = results
--   where
--   asts = extractAstInformation m
--   (moduleDeclarations, reexportRefs) = convertExterns e
--   results =
--         moduleDeclarations
--         & resolveDataConstructorsForModule
--         & resolveLocationsForModule asts
--         & resolveDocumentationForModule m
--         -- & resolveInstances externs
--         -- & resolveOperators
--         -- & resolveReexports reexportRefs
--
-- resolveLocations
--   :: ModuleMap (DefinitionSites P.SourceSpan, TypeAnnotations)
--   -> ModuleMap [IdeDeclarationAnn]
--   -> ModuleMap [IdeDeclarationAnn]
-- resolveLocations asts =
--   Map.mapWithKey (\mn decls ->
--                     maybe decls (flip resolveLocationsForModule decls) (Map.lookup mn asts))
--
-- resolveLocationsForModule
--   :: (DefinitionSites P.SourceSpan, TypeAnnotations)
--   -> [IdeDeclarationAnn]
--   -> [IdeDeclarationAnn]
-- resolveLocationsForModule (defs, types) =
--   map convertDeclaration
--   where
--     convertDeclaration :: IdeDeclarationAnn -> IdeDeclarationAnn
--     convertDeclaration (IdeDeclarationAnn ann d) = convertDeclaration'
--       annotateFunction
--       annotateValue
--       annotateDataConstructor
--       annotateType
--       annotateType -- type classes live in the type namespace
--       annotateModule
--       d
--       where
--         annotateFunction x = IdeDeclarationAnn (ann { _annLocation = Map.lookup (IdeNamespaced IdeNSValue (P.runIdent x)) defs
--                                                     , _annTypeAnnotation = Map.lookup x types
--                                                     })
--         annotateValue x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
--         annotateDataConstructor x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSValue x) defs})
--         annotateType x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSType x) defs})
--         annotateModule x = IdeDeclarationAnn (ann {_annLocation = Map.lookup (IdeNamespaced IdeNSModule x) defs})
--
-- convertDeclaration'
--   :: (P.Ident -> IdeDeclaration -> IdeDeclarationAnn)
--   -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
--   -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
--   -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
--   -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
--   -> (Text -> IdeDeclaration -> IdeDeclarationAnn)
--   -> IdeDeclaration
--   -> IdeDeclarationAnn
-- convertDeclaration' annotateFunction annotateValue annotateDataConstructor annotateType annotateClass annotateModule d =
--   case d of
--     IdeDeclValue v ->
--       annotateFunction (v ^. ideValueIdent) d
--     IdeDeclType t ->
--       annotateType (t ^. ideTypeName . properNameT) d
--     IdeDeclTypeSynonym s ->
--       annotateType (s ^. ideSynonymName . properNameT) d
--     IdeDeclDataConstructor dtor ->
--       annotateDataConstructor (dtor ^. ideDtorName . properNameT) d
--     IdeDeclTypeClass tc ->
--       annotateClass (tc ^. ideTCName . properNameT) d
--     IdeDeclValueOperator operator ->
--       annotateValue (operator ^. ideValueOpName . opNameT) d
--     IdeDeclTypeOperator operator ->
--       annotateType (operator ^. ideTypeOpName . opNameT) d
--     IdeDeclModule mn ->
--       annotateModule (P.runModuleName mn) d
--
-- resolveDocumentation
--   :: ModuleMap P.Module
--   -> ModuleMap [IdeDeclarationAnn]
--   -> ModuleMap [IdeDeclarationAnn]
-- resolveDocumentation modules =
--   Map.mapWithKey (\mn decls ->
--     maybe decls (flip resolveDocumentationForModule decls) (Map.lookup mn modules))
--
-- resolveDocumentationForModule
--   :: P.Module
--     -> [IdeDeclarationAnn]
--     -> [IdeDeclarationAnn]
-- resolveDocumentationForModule (P.Module _ moduleComments moduleName sdecls _) =
--   map convertDecl
--   where
--   extractDeclComments :: P.Declaration -> [(P.Name, [P.Comment])]
--   extractDeclComments = \case
--     P.DataDeclaration (_, cs) _ ctorName _ ctors ->
--       (P.TyName ctorName, cs) : map dtorComments ctors
--     P.TypeClassDeclaration (_, cs) tyClassName _ _ _ members ->
--       (P.TyClassName tyClassName, cs) : concatMap extractDeclComments members
--     decl ->
--       maybe [] (\name' -> [(name', snd (P.declSourceAnn decl))]) (name decl)
--
--   comments :: Map P.Name [P.Comment]
--   comments = Map.insert (P.ModName moduleName) moduleComments $
--     Map.fromListWith (flip (<>)) $ concatMap extractDeclComments sdecls
--
--   dtorComments :: P.DataConstructorDeclaration -> (P.Name, [P.Comment])
--   dtorComments dcd = (P.DctorName (P.dataCtorName dcd), snd (P.dataCtorAnn dcd))
--
--   name :: P.Declaration -> Maybe P.Name
--   name (P.TypeDeclaration d) = Just $ P.IdentName $ P.tydeclIdent d
--   name decl = P.declName decl
--
--   convertDecl :: IdeDeclarationAnn -> IdeDeclarationAnn
--   convertDecl (IdeDeclarationAnn ann d) =
--     convertDeclaration'
--       (annotateValue . P.IdentName)
--       (annotateValue . P.IdentName . P.Ident)
--       (annotateValue . P.DctorName . P.ProperName)
--       (annotateValue . P.TyName . P.ProperName)
--       (annotateValue . P.TyClassName . P.ProperName)
--       (annotateValue . P.ModName . P.moduleNameFromString)
--       d
--     where
--       docs :: P.Name -> Text
--       docs ident = fromMaybe "" $ convertComments =<< Map.lookup ident comments
--
--       annotateValue ident = IdeDeclarationAnn (ann { _annDocumentation = Just $ docs ident })
--
-- resolveInstances
--   :: ModuleMap P.ExternsFile
--   -> ModuleMap [IdeDeclarationAnn]
--   -> ModuleMap [IdeDeclarationAnn]
-- resolveInstances externs declarations =
--   Map.foldr (flip (foldr go)) declarations
--   . Map.mapWithKey (\mn ef -> mapMaybe (extractInstances mn) (efDeclarations ef))
--   $ externs
--   where
--     extractInstances mn P.EDInstance{..} =
--       case edInstanceClassName of
--           P.Qualified (P.ByModuleName classModule) className ->
--             Just (IdeInstance mn
--                   edInstanceName
--                   edInstanceTypes
--                   edInstanceConstraints, classModule, className)
--           _ -> Nothing
--     extractInstances _ _ = Nothing
--
--     go
--       :: (IdeInstance, P.ModuleName, P.ProperName 'P.ClassName)
--       -> ModuleMap [IdeDeclarationAnn]
--       -> ModuleMap [IdeDeclarationAnn]
--     go (ideInstance, classModule, className) acc' =
--       let
--         matchTC =
--           anyOf (idaDeclaration . _IdeDeclTypeClass . ideTCName) (== className)
--         updateDeclaration =
--           mapIf matchTC (idaDeclaration
--                          . _IdeDeclTypeClass
--                          . ideTCInstances
--                          %~ (ideInstance :))
--       in
--         acc' & ix classModule %~ updateDeclaration
--
-- resolveOperators
--   :: ModuleMap [IdeDeclarationAnn]
--   -> ModuleMap [IdeDeclarationAnn]
-- resolveOperators modules =
--   map (resolveOperatorsForModule modules) modules
--
-- -- | Looks up the types and kinds for operators and assigns them to their
-- -- declarations
-- resolveOperatorsForModule
--   :: ModuleMap [IdeDeclarationAnn]
--   -> [IdeDeclarationAnn]
--   -> [IdeDeclarationAnn]
-- resolveOperatorsForModule modules = map (idaDeclaration %~ resolveOperator)
--   where
--     getDeclarations :: P.ModuleName -> [IdeDeclaration]
--     getDeclarations moduleName =
--       Map.lookup moduleName modules
--       & foldMap (map discardAnn)
--
--     resolveOperator (IdeDeclValueOperator op)
--       | (P.Qualified (P.ByModuleName mn) (Left ident)) <- op ^. ideValueOpAlias =
--           let t = getDeclarations mn
--                   & mapMaybe (preview _IdeDeclValue)
--                   & filter (anyOf ideValueIdent (== ident))
--                   & map (view ideValueType)
--                   & listToMaybe
--           in IdeDeclValueOperator (op & ideValueOpType .~ t)
--       | (P.Qualified (P.ByModuleName mn) (Right dtor)) <- op ^. ideValueOpAlias =
--           let t = getDeclarations mn
--                   & mapMaybe (preview _IdeDeclDataConstructor)
--                   & filter (anyOf ideDtorName (== dtor))
--                   & map (view ideDtorType)
--                   & listToMaybe
--           in IdeDeclValueOperator (op & ideValueOpType .~ t)
--     resolveOperator (IdeDeclTypeOperator op)
--       | P.Qualified (P.ByModuleName mn) properName <- op ^. ideTypeOpAlias =
--           let k = getDeclarations mn
--                   & mapMaybe (preview _IdeDeclType)
--                   & filter (anyOf ideTypeName (== properName))
--                   & map (view ideTypeKind)
--                   & listToMaybe
--           in IdeDeclTypeOperator (op & ideTypeOpKind .~ k)
--     resolveOperator x = x
--
--
-- mapIf :: Functor f => (b -> Bool) -> (b -> b) -> f b -> f b
-- mapIf p f = map (\x -> if p x then f x else x)
--
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
--       & mapMaybe (preview (idaDeclaration . _IdeDeclDataConstructor))
--       & foldr (\(IdeDataConstructor name typeName type') ->
--                   Map.insertWith (<>) typeName [(name, type')]) Map.empty
