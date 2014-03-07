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

import Data.List (intercalate)

import qualified Data.Map as M

import Control.Monad.Writer

import Language.PureScript.Declarations
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Pretty
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Kinds

-- |
-- Generate foreign imports for all declarations in a module
--
moduleToPs :: Module -> Environment -> String
moduleToPs (Module moduleName _ exts) env = intercalate "\n" . execWriter $ do
  tell ["module " ++ runModuleName moduleName ++ " where"]
  let typesExported = getTypesExportedFrom moduleName exts env
  forM_ typesExported $ \(pn, kind) ->
    tell ["foreign import data " ++ show pn ++ " :: " ++ prettyPrintKind kind]
  let namesExported = getNamesExportedFrom moduleName exts env
  forM_ namesExported $ \(ident, ty) ->
    tell ["foreign import " ++ show ident ++ " :: " ++ prettyPrintType ty]

getNamesExportedFrom :: ModuleName -> Maybe [DeclarationRef] -> Environment -> [(Ident, Type)]
getNamesExportedFrom moduleName exps env =
  [ (ident, ty)
  | ((moduleName', ident), (ty, nameKind)) <- M.toList . names $ env
  , moduleName == moduleName'
  , nameKind `elem` [Value, Extern ForeignImport]
  , isExported ident exps
  ]
  where
  isExported :: Ident -> Maybe [DeclarationRef] -> Bool
  isExported _ Nothing = True
  isExported ident (Just exps') = ValueRef ident `elem` exps'

getTypesExportedFrom :: ModuleName -> Maybe [DeclarationRef] -> Environment -> [(ProperName, Kind)]
getTypesExportedFrom moduleName exps env =
  [ (pn, kind)
  | ((Qualified (Just moduleName') pn), kind) <- M.toList . types $ env
  , moduleName == moduleName'
  , isExported pn exps
  ]
  where
  isExported :: ProperName -> Maybe [DeclarationRef] -> Bool
  isExported _ Nothing = True
  isExported pn (Just exps') = flip any exps' $ \e -> case e of
    TypeRef pn' _ | pn == pn' -> True
    _ -> False
