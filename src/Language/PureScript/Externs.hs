{-# LANGUAGE TemplateHaskell #-}

-- |
-- This module generates code for \"externs\" files, i.e. files containing only
-- foreign import declarations.
--
module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  ) where

import Prelude.Compat

import Data.Aeson.TH
import Data.Foldable (fold)
import Data.List (find, foldl')
import Data.Maybe (mapMaybe, maybeToList, fromMaybe)
import Data.Version (showVersion)
import qualified Data.Map as M

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

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
  -- | List of type operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- | List of type and value declaration
  , efDeclarations :: [ExternsDeclaration]
  } deriving (Show)

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show)

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: OpName 'ValueOpName
  -- | The value the operator is an alias for
  , efAlias :: Qualified (Either Ident (ProperName 'ConstructorName))
  } deriving (Show)

-- | A type fixity declaration in an externs file
data ExternsTypeFixity = ExternsTypeFixity
  {
  -- | The associativity of the operator
    efTypeAssociativity :: Associativity
  -- | The precedence level of the operator
  , efTypePrecedence :: Precedence
  -- | The operator symbol
  , efTypeOperator :: OpName 'TypeOpName
  -- | The value the operator is an alias for
  , efTypeAlias :: Qualified (ProperName 'TypeName)
  } deriving (Show)

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: Kind
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(String, Maybe Kind)]
      , edTypeSynonymType         :: Type
      }
  -- | A data construtor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: Type
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: Type
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(String, Maybe Kind)]
      , edClassMembers            :: [(Ident, Type)]
      , edClassConstraints        :: [Constraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceTypes           :: [Type]
      , edInstanceConstraints     :: Maybe [Constraint]
      }
  deriving Show

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (Just efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps) = env { typeClasses = M.insert (qual pn) (TypeClassData args members cs deps) (typeClasses env) }
  applyDecl env (EDInstance className ident tys cs) = env { typeClassDictionaries = updateMap (updateMap (M.insert (qual ident) dict) className) (Just efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
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
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = concatMap toExternsDeclaration efExports

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration (Fixity assoc prec) name op) =
      fmap (const (ExternsFixity assoc prec op name)) (find (findOp getValueOpRef op) exps)
  fixityDecl (PositionedDeclaration _ _ d) = fixityDecl d
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration (Fixity assoc prec) name op) =
      fmap (const (ExternsTypeFixity assoc prec op name)) (find (findOp getTypeOpRef op) exps)
  typeFixityDecl (PositionedDeclaration _ _ d) = typeFixityDecl d
  typeFixityDecl _ = Nothing

  findOp :: (DeclarationRef -> Maybe (OpName a)) -> OpName a -> DeclarationRef -> Bool
  findOp get op = maybe False (== op) . get

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
                            , (dty, _, ty, args) <- maybeToList (Qualified (Just mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef ident)
    | Just (ty, _, _) <- Qualified (Just mn) ident `M.lookup` names env
    = [ EDValue ident ty ]
  toExternsDeclaration (TypeClassRef className)
    | Just TypeClassData{..} <- Qualified (Just mn) className `M.lookup` typeClasses env
    , Just (kind, TypeSynonym) <- Qualified (Just mn) (coerceProperName className) `M.lookup` types env
    , Just (_, synTy) <- Qualified (Just mn) (coerceProperName className) `M.lookup` typeSynonyms env
    = [ EDType (coerceProperName className) kind TypeSynonym
      , EDTypeSynonym (coerceProperName className) typeClassArguments synTy
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies
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
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsTypeFixity)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsDeclaration)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsFile)
