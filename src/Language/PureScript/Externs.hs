-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Externs
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code for \"externs\" files, i.e. files containing only foreign import declarations.
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsDeclaration(..)
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  ) where

import Data.List (find, foldl')
import Data.Maybe (mapMaybe, maybeToList, fromMaybe)
import Data.Foldable (fold)
import Data.Version (showVersion)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Aeson.TH

import qualified Data.Map as M

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Kinds
import Language.PureScript.TypeClassDictionaries

import Paths_purescript as Paths

-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  {
  -- | The externs version
    efVersion :: String
  -- | Module name
  , efModuleName :: ModuleName
  -- | List of module exports
  , efExports :: [DeclarationRef]
  -- | List of module imports
  , efImports :: [ExternsImport]
  -- | List of operators and their fixities
  , efFixities :: [ExternsFixity]
  -- | List of type and value declaration
  , efDeclarations :: [ExternsDeclaration]
  } deriving (Show, Read)

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Read)

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: String
  } deriving (Show, Read)

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName :: ProperName
      , edTypeKind :: Kind
      , edTypeDeclarationKind :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName :: ProperName
      , edTypeSynonymArguments :: [(String, Maybe Kind)]
      , edTypeSynonymType :: Type
      }
  -- | A data construtor
  | EDDataConstructor
      { edDataCtorName :: ProperName
      , edDataCtorOrigin :: DataDeclType
      , edDataCtorTypeCtor :: ProperName
      , edDataCtorType :: Type
      , edDataCtorFields :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName :: Ident
      , edValueType :: Type
      }
  -- | A type class declaration
  | EDClass
      { edClassName :: ProperName
      , edClassTypeArguments :: [(String, Maybe Kind)]
      , edClassMembers :: [(Ident, Type)]
      , edClassConstraints :: [Constraint]
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName :: Qualified ProperName
      , edInstanceName :: Ident
      , edInstanceTypes :: [Type]
      , edInstanceConstraints :: Maybe [Constraint]
      }
  deriving (Show, Read)

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (efModuleName, ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs) = env { typeClasses = M.insert (qual pn) (args, members, cs) (typeClasses env) }
  applyDecl env (EDInstance className ident tys cs) = env { typeClassDictionaries = updateMap (updateMap (M.insert (qual ident) dict) className) (Just efModuleName) (typeClassDictionaries env) }
    where
    dict :: TypeClassDictionaryInScope
    dict = TypeClassDictionaryInScope (qual ident) [] className tys cs

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

  qual :: a -> Qualified a
  qual = Qualified (Just efModuleName)

-- | Generate an externs file for all declarations in a module
moduleToExternsFile :: Module -> Environment -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ = internalError "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile (Module _ _ mn ds (Just exps)) env = ExternsFile{..}
  where
  efVersion       = showVersion Paths.version
  efModuleName    = mn
  efExports       = exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efDeclarations  = concatMap toExternsDeclaration efExports

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (FixityDeclaration (Fixity assoc prec) op) = fmap (const (ExternsFixity assoc prec op)) (find exportsOp exps)
    where
    exportsOp :: DeclarationRef -> Bool
    exportsOp (PositionedDeclarationRef _ _ r) = exportsOp r
    exportsOp (ValueRef ident') = ident' == Op op
    exportsOp _ = False
  fixityDecl (PositionedDeclaration _ _ d) = fixityDecl d
  fixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl (PositionedDeclaration _ _ d) = importDecl d
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (PositionedDeclarationRef _ _ r) = toExternsDeclaration r
  toExternsDeclaration (TypeRef pn dctors) =
    case Qualified (Just mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (Just mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData) -> [ EDType pn kind ExternData ]
      Just (kind, tk@(DataType _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (M.lookup (Qualified (Just mn) dctor) (dataConstructors env))
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef ident)
    | Just (ty, _, _) <- (mn, ident) `M.lookup` names env
    = [ EDValue ident ty ]
  toExternsDeclaration (TypeClassRef className)
    | Just (args, members, implies) <- Qualified (Just mn) className `M.lookup` typeClasses env
    , Just (kind, TypeSynonym) <- M.lookup (Qualified (Just mn) className) (types env)
    , Just (_, synTy) <- Qualified (Just mn) className `M.lookup` typeSynonyms env
    = [ EDType className kind TypeSynonym
      , EDTypeSynonym className args synTy
      , EDClass className args members implies
      ]
  toExternsDeclaration (TypeInstanceRef ident)
    = [ EDInstance tcdClassName ident tcdInstanceTypes tcdDependencies
      | m1 <- maybeToList (M.lookup (Just mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , TypeClassDictionaryInScope{..} <- maybeToList (M.lookup (Qualified (Just mn) ident) m2)
      ]
  toExternsDeclaration _ = []

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsImport)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsFixity)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsDeclaration)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsFile)
