-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.Externs
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code for \"externs\" files, i.e. files containing only foreign import declarations.
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen.Externs (
    moduleToPs
) where

import Data.List (intercalate, find)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M

import Control.Monad.Writer

import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import Language.PureScript.Comments

-- |
-- Generate foreign imports for all declarations in a module
--
moduleToPs :: Module -> Environment -> String
moduleToPs (Module _ _ Nothing) _ = error "Module exports were not elaborated in moduleToPs"
moduleToPs (Module moduleName ds (Just exts)) env = intercalate "\n" . execWriter $ do
  tell [ "module " ++ runModuleName moduleName ++ " where"]
  mapM_ declToPs ds
  mapM_ exportToPs exts
  where

    declToPs :: Declaration -> Writer [String] ()
    declToPs (ImportDeclaration mn _ _) = tell ["import " ++ show mn ++ " ()"]
    declToPs (FixityDeclaration (Fixity assoc prec) op) =
      case find exportsOp exts of
        Nothing -> return ()
        Just _ -> tell [ unwords [ show assoc, show prec, op ] ]
      where
      exportsOp :: DeclarationRef -> Bool
      exportsOp (PositionedDeclarationRef _ _ r) = exportsOp r
      exportsOp (ValueRef ident') = ident' == Op op
      exportsOp _ = False
    declToPs (PositionedDeclaration _ com d) = mapM commentToPs com >> declToPs d
    declToPs _ = return ()

    commentToPs :: Comment -> Writer [String] ()
    commentToPs (LineComment s) = tell ["-- " ++ s]
    commentToPs (BlockComment s) = tell ["{- " ++ s ++ " -}"]

    exportToPs :: DeclarationRef -> Writer [String] ()
    exportToPs (PositionedDeclarationRef _ _ r) = exportToPs r
    exportToPs (TypeRef pn dctors) = do
      case Qualified (Just moduleName) pn `M.lookup` types env of
        Nothing -> error $ show pn ++ " has no kind in exportToPs"
        Just (kind, ExternData) ->
          tell ["foreign import data " ++ show pn ++ " :: " ++ prettyPrintKind kind]
        Just (_, DataType args tys) -> do
          let dctors' = fromMaybe (map fst tys) dctors
              printDctor dctor = case dctor `lookup` tys of
                                   Nothing -> Nothing
                                   Just tyArgs -> Just $ show dctor ++ " " ++ unwords (map prettyPrintTypeAtom tyArgs)
          let dtype = if length dctors' == 1 && isNewtypeConstructor env (Qualified (Just moduleName) $ head dctors')
                      then "newtype"
                      else "data"
              typeName = prettyPrintType $ foldl TypeApp (TypeConstructor (Qualified Nothing pn)) (map toTypeVar args)
          tell [dtype ++ " " ++ typeName ++ (if null dctors' then "" else " = " ++ intercalate " | " (mapMaybe printDctor dctors'))]
        Just (_, TypeSynonym) ->
          case Qualified (Just moduleName) pn `M.lookup` typeSynonyms env of
            Nothing -> error $ show pn ++ " has no type synonym info in exportToPs"
            Just (args, synTy) ->
              let
                typeName = prettyPrintType $ foldl TypeApp (TypeConstructor (Qualified Nothing pn)) (map toTypeVar args)
              in tell ["type " ++ typeName ++ " = " ++ prettyPrintType synTy]
        _ -> error "Invalid input in exportToPs"

    exportToPs (ValueRef ident) =
      case (moduleName, ident) `M.lookup` names env of
        Nothing -> error $ show ident ++ " has no type in exportToPs"
        Just (ty, nameKind, _) | nameKind == Value || nameKind == Extern ForeignImport || nameKind == Extern InlineJavascript ->
          tell ["foreign import " ++ show ident ++ " :: " ++ prettyPrintType ty]
        _ -> return ()
    exportToPs (TypeClassRef className) =
      case Qualified (Just moduleName) className `M.lookup` typeClasses env of
        Nothing -> error $ show className ++ " has no type class definition in exportToPs"
        Just (args, members, implies) -> do
          let impliesString = if null implies
                              then ""
                              else "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map prettyPrintTypeAtom tys')) implies) ++ ") <= "
              typeName = prettyPrintType $ foldl TypeApp (TypeConstructor (Qualified Nothing className)) (map toTypeVar args)
          tell ["class " ++ impliesString ++ typeName ++ " where"]
          forM_ (filter (isValueExported . fst) members) $ \(member ,ty) ->
            tell [ "  " ++ show member ++ " :: " ++ prettyPrintType ty ]

    exportToPs (TypeInstanceRef ident) = do
      let TypeClassDictionaryInScope { tcdClassName = className, tcdInstanceTypes = tys, tcdDependencies = deps} =
            fromMaybe (error $ "Type class instance has no dictionary in exportToPs") . find (\tcd -> tcdName tcd == Qualified (Just moduleName) ident && tcdType tcd == TCDRegular) $ M.elems $ typeClassDictionaries env
      let constraintsText = case fromMaybe [] deps of
                              [] -> ""
                              cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map prettyPrintTypeAtom tys')) cs) ++ ") => "
      tell ["foreign import instance " ++ show ident ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map prettyPrintTypeAtom tys)]

    toTypeVar :: (String, Maybe Kind) -> Type
    toTypeVar (s, Nothing) = TypeVar s
    toTypeVar (s, Just k) = KindedType (TypeVar s) k

    isValueExported :: Ident -> Bool
    isValueExported ident = ValueRef ident `elem` exts
