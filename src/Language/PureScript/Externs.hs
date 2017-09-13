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
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Set as S

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
  { efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
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
      , edTypeSynonymArguments    :: [(Text, Maybe Kind)]
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
      , edClassTypeArguments      :: [(Text, Maybe Kind)]
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
      , edInstanceChain           :: [Qualified Ident]
      , edInstanceChainIndex      :: Integer
      }
  -- | A kind declaration
  | EDKind
      { edKindName                :: ProperName 'KindName
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
  applyDecl env (EDClass pn args members cs deps) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps) (typeClasses env) }
  applyDecl env (EDKind pn) = env { kinds = S.insert (qual pn) (kinds env) }
  applyDecl env (EDInstance className ident tys cs ch idx) = env { typeClassDictionaries = updateMap (updateMap (M.insert (qual ident) dict) className) (Just efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className tys cs

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

  qual :: a -> Qualified a
  qual = Qualified (Just efModuleName)

-- | Generate an externs file for all declarations in a module
moduleToExternsFile :: Module -> Environment -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ = internalError "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile (Module ss _ mn ds (Just exps)) env = ExternsFile{..}
  where
  efVersion       = T.pack (showVersion Paths.version)
  efModuleName    = mn
  efExports       = exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = concatMap toExternsDeclaration efExports
  efSourceSpan    = ss

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find (findOp getValueOpRef op) exps)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find (findOp getTypeOpRef op) exps)
  typeFixityDecl _ = Nothing

  findOp :: (DeclarationRef -> Maybe (OpName a)) -> OpName a -> DeclarationRef -> Bool
  findOp g op = maybe False (== op) . g

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (TypeRef _ pn dctors) =
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
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (Just mn) ident `M.lookup` names env
    = [ EDValue ident ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | Just TypeClassData{..} <- Qualified (Just mn) className `M.lookup` typeClasses env
    , Just (kind, TypeSynonym) <- Qualified (Just mn) (coerceProperName className) `M.lookup` types env
    , Just (_, synTy) <- Qualified (Just mn) (coerceProperName className) `M.lookup` typeSynonyms env
    = [ EDType (coerceProperName className) kind TypeSynonym
      , EDTypeSynonym (coerceProperName className) typeClassArguments synTy
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies
      ]
  toExternsDeclaration (TypeInstanceRef _ ident)
    = [ EDInstance tcdClassName ident tcdInstanceTypes tcdDependencies tcdChain tcdIndex
      | m1 <- maybeToList (M.lookup (Just mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , TypeClassDictionaryInScope{..} <- maybeToList (M.lookup (Qualified (Just mn) ident) m2)
      ]
  toExternsDeclaration (KindRef _ pn)
    | Qualified (Just mn) pn `S.member` kinds env
    = [ EDKind pn ]
  toExternsDeclaration _ = []

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsImport)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsFixity)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsTypeFixity)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsDeclaration)
$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ExternsFile)
