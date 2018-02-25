module Language.PureScript.Interactive.Printer where

import           Prelude.Compat

import           Data.List (intersperse, find)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Language.PureScript as P
import qualified Text.PrettyPrint.Boxes as Box
import           Language.PureScript.Interactive.Types
import           Language.PureScript.AST.Declarations
import           Language.PureScript.Environment
-- TODO (Christoph): Text version of boxes
textT :: Text -> Box.Box
textT = Box.text . T.unpack

-- Printers

-- |
-- Pretty print a module's signatures
--
printModuleSignatures :: P.ModuleName -> PSCiConfig -> PSCiState -> String
printModuleSignatures moduleName cfg@PSCiConfig{..} st@PSCiState{..} = 
    -- get relevant components of a module from environment
    let env = psciEnvironment
        moduleNamesIdent = byModuleName (names env)
        moduleTypeClasses = byModuleName (typeClasses env)
        moduleTypes = byModuleName (types env)
        byModuleName = filter ((== Just moduleName) . P.getQual) . M.keys
         
        moduleNamesExported = case getModule moduleName of 
                            Just modul -> [ modName | (ModuleRef _ modName) <- P.getModuleDeclarationRefs modul]
                            Nothing -> []
        modulePrintRepresentation = -- print each component
          (unlines . map trimEnd . lines . Box.render . Box.vsep 1 Box.left)
            [ printModule's (mapMaybe (showTypeClass . findTypeClass (typeClasses env))) moduleTypeClasses -- typeClasses
            , printModule's (mapMaybe (showType (typeClasses env) (dataConstructors env) (typeSynonyms env) . findType (types env))) moduleTypes -- types
            , printModule's (map (showNameType . findNameType (names env))) moduleNamesIdent -- functions
            ]
    in  modulePrintRepresentation ++ concat [ printModuleSignatures mn cfg st | mn <- moduleNamesExported]
    
  where printModule's showF = Box.vsep 1 Box.left . showF
        getModule modName = 
          let mMod = find (\(m, _) -> P.getModuleName m == modName) psciLoadedExterns        
          in case mMod of 
            Just (modu, _) -> Just modu
            Nothing -> Nothing -- This should never happen since `handleBrowse` makes sure to always send us something that is a valid module. TODO: Fix this assumption, don't rely on how external code works.

        findNameType :: M.Map (P.Qualified P.Ident) (P.Type, P.NameKind, P.NameVisibility)
                     -> P.Qualified P.Ident
                     -> (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility))
        findNameType envNames m = (P.disqualify m, M.lookup m envNames)

        showNameType :: (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility)) -> Box.Box
        showNameType (mIdent, Just (mType, _, _)) = textT (P.showIdent mIdent <> " :: ") Box.<> P.typeAsBox mType
        showNameType _ = P.internalError "The impossible happened in printModuleSignatures."

        findTypeClass
          :: M.Map (P.Qualified (P.ProperName 'P.ClassName)) P.TypeClassData
          -> P.Qualified (P.ProperName 'P.ClassName)
          -> (P.Qualified (P.ProperName 'P.ClassName), Maybe P.TypeClassData)
        findTypeClass envTypeClasses name = (name, M.lookup name envTypeClasses)

        showTypeClass
          :: (P.Qualified (P.ProperName 'P.ClassName), Maybe P.TypeClassData)
          -> Maybe Box.Box
        showTypeClass (_, Nothing) = Nothing
        showTypeClass (P.Qualified _ name, Just P.TypeClassData{..}) =
            let constraints =
                    if null typeClassSuperclasses
                    then Box.text ""
                    else Box.text "("
                         Box.<> Box.hcat Box.left (intersperse (Box.text ", ") $ map (\(P.Constraint (P.Qualified _ pn) lt _) -> textT (P.runProperName pn) Box.<+> Box.hcat Box.left (map P.typeAtomAsBox lt)) typeClassSuperclasses)
                         Box.<> Box.text ") <= "
                className =
                    textT (P.runProperName name)
                    Box.<> textT (foldMap ((" " <>) . fst) typeClassArguments)
                classBody =
                    Box.vcat Box.top (map (\(i, t) -> textT (P.showIdent i <> " ::") Box.<+> P.typeAsBox t) typeClassMembers)

            in
              Just $
                (Box.text "class "
                Box.<> constraints
                Box.<> className
                Box.<+> if null typeClassMembers then Box.text "" else Box.text "where")
                Box.// Box.moveRight 2 classBody


        findType
          :: M.Map (P.Qualified (P.ProperName 'P.TypeName)) (P.Kind, P.TypeKind)
          -> P.Qualified (P.ProperName 'P.TypeName)
          -> (P.Qualified (P.ProperName 'P.TypeName), Maybe (P.Kind, P.TypeKind))
        findType envTypes name = (name, M.lookup name envTypes)

        showType
          :: M.Map (P.Qualified (P.ProperName 'P.ClassName)) P.TypeClassData
          -> M.Map (P.Qualified (P.ProperName 'P.ConstructorName)) (P.DataDeclType, P.ProperName 'P.TypeName, P.Type, [P.Ident])
          -> M.Map (P.Qualified (P.ProperName 'P.TypeName)) ([(Text, Maybe P.Kind)], P.Type)
          -> (P.Qualified (P.ProperName 'P.TypeName), Maybe (P.Kind, P.TypeKind))
          -> Maybe Box.Box
        showType typeClassesEnv dataConstructorsEnv typeSynonymsEnv (n@(P.Qualified modul name), typ) =
          case (typ, M.lookup n typeSynonymsEnv) of
            (Just (_, P.TypeSynonym), Just (typevars, dtType)) ->
                if M.member (fmap P.coerceProperName n) typeClassesEnv
                then
                  Nothing
                else
                  Just $
                    textT ("type " <> P.runProperName name <> foldMap ((" " <>) . fst) typevars)
                    Box.// Box.moveRight 2 (Box.text "=" Box.<+> P.typeAsBox dtType)

            (Just (_, P.DataType typevars pt), _) ->
              let prefix =
                    case pt of
                      [(dtProperName,_)] ->
                        case M.lookup (P.Qualified modul dtProperName) dataConstructorsEnv of
                          Just (dataDeclType, _, _, _) -> P.showDataDeclType dataDeclType
                          _ -> "data"
                      _ -> "data"

              in
                Just $ textT (prefix <> " " <> P.runProperName name <> foldMap ((" " <>) . fst) typevars) Box.// printCons pt

            _ ->
              Nothing

          where printCons pt =
                    Box.moveRight 2 $
                    Box.vcat Box.left $
                    mapFirstRest (Box.text "=" Box.<+>) (Box.text "|" Box.<+>) $
                    map (\(cons,idents) -> (textT (P.runProperName cons) Box.<> Box.hcat Box.left (map prettyPrintType idents))) pt

                prettyPrintType t = Box.text " " Box.<> P.typeAtomAsBox t

                mapFirstRest _ _ [] = []
                mapFirstRest f g (x:xs) = f x : map g xs

        trimEnd = reverse . dropWhile (== ' ') . reverse
 
        
        {-
        printModuleSignatures :: P.ModuleName -> P.Environment -> String
printModuleSignatures moduleName P.Environment{..} = 
    -- get relevant components of a module from environment
    let moduleNamesIdent = byModuleName names
        moduleTypeClasses = byModuleName typeClasses
        moduleTypes = byModuleName types
        byModuleName = filter ((== Just moduleName) . P.getQual) . M.keys

    in
    -- print each component
    (unlines . map trimEnd . lines . Box.render . Box.vsep 1 Box.left)
      [ printModule's (mapMaybe (showTypeClass . findTypeClass typeClasses)) moduleTypeClasses -- typeClasses
      , printModule's (mapMaybe (showType typeClasses dataConstructors typeSynonyms . findType types)) moduleTypes -- types
      , printModule's (map (showNameType . findNameType names)) moduleNamesIdent -- functions
      ]

  where printModule's showF = Box.vsep 1 Box.left . showF

        findNameType :: M.Map (P.Qualified P.Ident) (P.Type, P.NameKind, P.NameVisibility)
                     -> P.Qualified P.Ident
                     -> (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility))
        findNameType envNames m = (P.disqualify m, M.lookup m envNames)

        showNameType :: (P.Ident, Maybe (P.Type, P.NameKind, P.NameVisibility)) -> Box.Box
        showNameType (mIdent, Just (mType, _, _)) = textT (P.showIdent mIdent <> " :: ") Box.<> P.typeAsBox mType
        showNameType _ = P.internalError "The impossible happened in printModuleSignatures."

        findTypeClass
          :: M.Map (P.Qualified (P.ProperName 'P.ClassName)) P.TypeClassData
          -> P.Qualified (P.ProperName 'P.ClassName)
          -> (P.Qualified (P.ProperName 'P.ClassName), Maybe P.TypeClassData)
        findTypeClass envTypeClasses name = (name, M.lookup name envTypeClasses)

        showTypeClass
          :: (P.Qualified (P.ProperName 'P.ClassName), Maybe P.TypeClassData)
          -> Maybe Box.Box
        showTypeClass (_, Nothing) = Nothing
        showTypeClass (P.Qualified _ name, Just P.TypeClassData{..}) =
            let constraints =
                    if null typeClassSuperclasses
                    then Box.text ""
                    else Box.text "("
                         Box.<> Box.hcat Box.left (intersperse (Box.text ", ") $ map (\(P.Constraint (P.Qualified _ pn) lt _) -> textT (P.runProperName pn) Box.<+> Box.hcat Box.left (map P.typeAtomAsBox lt)) typeClassSuperclasses)
                         Box.<> Box.text ") <= "
                className =
                    textT (P.runProperName name)
                    Box.<> textT (foldMap ((" " <>) . fst) typeClassArguments)
                classBody =
                    Box.vcat Box.top (map (\(i, t) -> textT (P.showIdent i <> " ::") Box.<+> P.typeAsBox t) typeClassMembers)

            in
              Just $
                (Box.text "class "
                Box.<> constraints
                Box.<> className
                Box.<+> if null typeClassMembers then Box.text "" else Box.text "where")
                Box.// Box.moveRight 2 classBody


        findType
          :: M.Map (P.Qualified (P.ProperName 'P.TypeName)) (P.Kind, P.TypeKind)
          -> P.Qualified (P.ProperName 'P.TypeName)
          -> (P.Qualified (P.ProperName 'P.TypeName), Maybe (P.Kind, P.TypeKind))
        findType envTypes name = (name, M.lookup name envTypes)

        showType
          :: M.Map (P.Qualified (P.ProperName 'P.ClassName)) P.TypeClassData
          -> M.Map (P.Qualified (P.ProperName 'P.ConstructorName)) (P.DataDeclType, P.ProperName 'P.TypeName, P.Type, [P.Ident])
          -> M.Map (P.Qualified (P.ProperName 'P.TypeName)) ([(Text, Maybe P.Kind)], P.Type)
          -> (P.Qualified (P.ProperName 'P.TypeName), Maybe (P.Kind, P.TypeKind))
          -> Maybe Box.Box
        showType typeClassesEnv dataConstructorsEnv typeSynonymsEnv (n@(P.Qualified modul name), typ) =
          case (typ, M.lookup n typeSynonymsEnv) of
            (Just (_, P.TypeSynonym), Just (typevars, dtType)) ->
                if M.member (fmap P.coerceProperName n) typeClassesEnv
                then
                  Nothing
                else
                  Just $
                    textT ("type " <> P.runProperName name <> foldMap ((" " <>) . fst) typevars)
                    Box.// Box.moveRight 2 (Box.text "=" Box.<+> P.typeAsBox dtType)

            (Just (_, P.DataType typevars pt), _) ->
              let prefix =
                    case pt of
                      [(dtProperName,_)] ->
                        case M.lookup (P.Qualified modul dtProperName) dataConstructorsEnv of
                          Just (dataDeclType, _, _, _) -> P.showDataDeclType dataDeclType
                          _ -> "data"
                      _ -> "data"

              in
                Just $ textT (prefix <> " " <> P.runProperName name <> foldMap ((" " <>) . fst) typevars) Box.// printCons pt

            _ ->
              Nothing

          where printCons pt =
                    Box.moveRight 2 $
                    Box.vcat Box.left $
                    mapFirstRest (Box.text "=" Box.<+>) (Box.text "|" Box.<+>) $
                    map (\(cons,idents) -> (textT (P.runProperName cons) Box.<> Box.hcat Box.left (map prettyPrintType idents))) pt

                prettyPrintType t = Box.text " " Box.<> P.typeAtomAsBox t

                mapFirstRest _ _ [] = []
                mapFirstRest f g (x:xs) = f x : map g xs

        trimEnd = reverse . dropWhile (== ' ') . reverse
        
        -}