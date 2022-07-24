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
  , BuildCacheFile(..)
  , ExternCacheKey(..)
  , DeclarationCacheRef(..)
  , BuildCacheDb
  , externsIsCurrentVersion
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  , externsFileName
  ) where

import Prelude.Compat

import Codec.Serialise (Serialise, serialise)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NEL
import Data.Function ((&))

import Language.PureScript.AST
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import Paths_purescript as Paths

import qualified Data.ByteString.Lazy as B


-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
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
  , efBuildCache :: BuildCacheFile
  } deriving (Show, Generic)

instance Serialise ExternsFile

-- TODO[drathier]: is it enough to look at just the cacheDeclarations (what we export)? or do we need to look at the cached imports too?

-- -- | The data which will be serialized to a build cache file
data BuildCacheFile = BuildCacheFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
  { bcVersion :: Text
  -- ^ The externs version
  , bcModuleName :: ModuleName
  -- ^ Module name
  -- NOTE[drathier]: using bytestrings here for faster encoding/decoding
  -- , bcCacheDeclarations :: M.Map DeclarationCacheRef [ExternCacheKey]
  , bcCacheDeclarations :: M.Map B.ByteString [B.ByteString]
  -- ^ Duplicated to avoid having to update all usages
  -- , bcCacheImports :: M.Map ModuleName (M.Map DeclarationCacheRef [ExternCacheKey])
  , bcCacheImports :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])
  -- ^ all declarations explicitly imported from a specific module, if explicitly imported
  -- 1. open imports are not cached, for now, since we don't know which things are used
  -- 2. hiding imports are also not cached, even though we could skip the hidden things and treat it as a closed import afterwards
  -- 3. explicit imports are matched to see if anything differs
  -- 4. if an imported modulename is missing, it's a new import or something we couldn't cache, so treat it as a cache miss
  -- 5. [ExternsDeclaration] is the list of extdecls we saw when building this module/externsfile; it's what we should compare against when looking for cache hits
  } deriving (Show, Generic)
instance Serialise BuildCacheFile

type BuildCacheDb = M.Map ModuleName BuildCacheFile

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Generic)

instance Serialise ExternsImport

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
  } deriving (Show, Generic)

instance Serialise ExternsFixity

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
  } deriving (Show, Generic)

instance Serialise ExternsTypeFixity

-- | A type or value declaration appearing in an externs file
data ExternsDeclaration =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: SourceType
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe SourceType)]
      , edTypeSynonymType         :: SourceType
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: SourceType
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: SourceType
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe SourceType)]
      , edClassMembers            :: [(Ident, SourceType)]
      , edClassConstraints        :: [SourceConstraint]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, SourceType)]
      , edInstanceKinds           :: [SourceType]
      , edInstanceTypes           :: [SourceType]
      , edInstanceConstraints     :: Maybe [SourceConstraint]
      , edInstanceChain           :: Maybe ChainId
      , edInstanceChainIndex      :: Integer
      , edInstanceNameSource      :: NameSource
      , edInstanceSourceSpan      :: SourceSpan
      }
  deriving (Show, Generic)

instance Serialise ExternsDeclaration

-- | Check whether the version in an externs file matches the currently running
-- version.
externsIsCurrentVersion :: ExternsFile -> Bool
externsIsCurrentVersion ef =
  T.unpack (efVersion ef) == showVersion Paths.version

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ExternsFile{..} = flip (foldl' applyDecl) efDeclarations
  where
  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (Just efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx ns ss) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (Just efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className vars kinds tys cs instTy

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

    instTy :: Maybe SourceType
    instTy = case ns of
      CompilerNamed -> Just $ srcInstanceType ss vars className tys
      UserNamed -> Nothing

  qual :: a -> Qualified a
  qual = Qualified (Just efModuleName)


_efBuildCache = efBuildCache
_bcCacheDeclarations = bcCacheDeclarations

data DeclarationCacheRef
  -- |
  -- A type class
  --
  = DeclCacheTypeClassRef (ProperName 'ClassName)
  -- |
  -- A type operator
  --
  | DeclCacheTypeOpRef (OpName 'TypeOpName)
  -- |
  -- A type constructor with data constructors
  --
  | DeclCacheTypeRef (ProperName 'TypeName) (Maybe [ProperName 'ConstructorName])
  -- |
  -- A value
  --
  | DeclCacheValueRef Ident
  -- |
  -- A value-level operator
  --
  | DeclCacheValueOpRef (OpName 'ValueOpName)
  -- |
  -- A type class instance, created during typeclass desugaring
  --
  | DeclCacheTypeInstanceRef Ident NameSource
  -- |
  -- A module, in its entirety
  --
  | DeclCacheModuleRef ModuleName
  -- |
  -- A value re-exported from another module. These will be inserted during
  -- elaboration in name desugaring.
  --
  | DeclCacheReExportRef ExportSource DeclarationRef
  deriving (Show, Generic)

instance Serialise DeclarationCacheRef

declRefToCacheRef :: DeclarationRef -> DeclarationCacheRef
declRefToCacheRef = \case
  TypeClassRef _ className -> DeclCacheTypeClassRef className
  TypeOpRef _ typeOpName -> DeclCacheTypeOpRef typeOpName
  TypeRef _ typeName mConstructorNames -> DeclCacheTypeRef typeName mConstructorNames
  ValueRef _ ident -> DeclCacheValueRef ident
  ValueOpRef _ valueOpName -> DeclCacheValueOpRef valueOpName
  TypeInstanceRef _ ident nameSource -> DeclCacheTypeInstanceRef ident nameSource
  ModuleRef _ moduleName -> DeclCacheModuleRef moduleName
  ReExportRef _ exportSource declarationRef -> DeclCacheReExportRef exportSource declarationRef

data ExternCacheKey =
  -- | A type declaration
    CacheEDType
      { cacheEdTypeName                :: ProperName 'TypeName
      , cacheEdTypeKind                :: Type ()
      -- , cacheEdTypeDeclarationKind     :: TypeKind -- contains SourceType for adt's, can we safely skip the entire field?
      }
  -- | A type synonym
  | CacheEDTypeSynonym
      { cacheEdTypeSynonymName         :: ProperName 'TypeName
      , cacheEdTypeSynonymArguments    :: [(Text, Maybe (Type ()))]
      , cacheEdTypeSynonymType         :: Type ()
      }
  -- | A data constructor
  | CacheEDDataConstructor
      { cacheEdDataCtorName            :: ProperName 'ConstructorName
      , cacheEdDataCtorOrigin          :: DataDeclType
      , cacheEdDataCtorTypeCtor        :: ProperName 'TypeName
      , cacheEdDataCtorType            :: Type ()
      , cacheEdDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | CacheEDValue
      { cacheEdValueName               :: Ident
      , cacheEdValueType               :: Type ()
      }
  -- | A type class declaration
  | CacheEDClass
      { cacheEdClassName               :: ProperName 'ClassName
      , cacheEdClassTypeArguments      :: [(Text, Maybe (Type ()))]
      , cacheEdClassMembers            :: [(Ident, (Type ()))]
      , cacheEdClassConstraints        :: [Constraint ()]
      , cacheEdFunctionalDependencies  :: [FunctionalDependency]
      , cacheEdIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | CacheEDInstance
      { cacheEdInstanceClassName       :: Qualified (ProperName 'ClassName)
      , cacheEdInstanceName            :: Ident
      , cacheEdInstanceForAll          :: [(Text, Type ())]
      , cacheEdInstanceKinds           :: [Type ()]
      , cacheEdInstanceTypes           :: [Type ()]
      , cacheEdInstanceConstraints     :: Maybe [Constraint ()]
      , cacheEdInstanceChain           :: Maybe ChainId -- contains sourcepos, can we skip it?
      , cacheEdInstanceChainIndex      :: Integer
      , cacheEdInstanceNameSource      :: NameSource
      -- , cacheEdInstanceSourceSpan      :: SourceSpan
      }
  deriving (Show, Generic)

instance Serialise ExternCacheKey

extDeclToCacheKey :: ExternsDeclaration -> ExternCacheKey
extDeclToCacheKey = \case
    EDType
      edTypeName             --   :: ProperName 'TypeName
      edTypeKind             --   :: Type ()
      _ -- (edTypeDeclarationKind     :: TypeKind) -- contains SourceType for adt's, can we safely skip the entire field?
       ->
        CacheEDType
          edTypeName
          (const () <$> edTypeKind)
    -- | A type synonym
    EDTypeSynonym
        edTypeSynonymName       --  :: ProperName 'TypeName
        edTypeSynonymArguments  --  :: [(Text, Maybe (Type ()))]
        edTypeSynonymType       --  :: Type ()
      -> CacheEDTypeSynonym
          edTypeSynonymName
          (fmap (fmap (fmap (const ()))) <$> edTypeSynonymArguments)
          (const () <$> edTypeSynonymType)
    -- | A data constructor
    EDDataConstructor
        edDataCtorName          --  :: ProperName 'ConstructorName
        edDataCtorOrigin        --  :: DataDeclType
        edDataCtorTypeCtor      --  :: ProperName 'TypeName
        edDataCtorType          --  :: Type ()
        edDataCtorFields        --  :: [Ident]
      -> CacheEDDataConstructor
          edDataCtorName
          edDataCtorOrigin
          edDataCtorTypeCtor
          (const () <$> edDataCtorType)
          edDataCtorFields
    -- | A value declaration
    EDValue
        edValueName        --  :: Ident
        edValueType        --  :: Type ()
      -> CacheEDValue
          edValueName
          (const () <$> edValueType)
    -- | A type class declaration
    EDClass
        edClassName              --  :: ProperName 'ClassName
        edClassTypeArguments     --  :: [(Text, Maybe (Type ()))]
        edClassMembers           --  :: [(Ident, Type ())]
        edClassConstraints       --  :: [Constraint ()]
        edFunctionalDependencies --  :: [FunctionalDependency]
        edIsEmpty                --  :: Bool
      -> CacheEDClass
          edClassName
          (fmap (fmap (fmap (const ()))) <$> edClassTypeArguments)
          (fmap (fmap (const ())) <$> edClassMembers)
          (fmap (const ()) <$> edClassConstraints)
          edFunctionalDependencies
          edIsEmpty
    -- | An instance declaration
    EDInstance
        edInstanceClassName     --   :: Qualified (ProperName 'ClassName)
        edInstanceName          --   :: Ident
        edInstanceForAll        --   :: [(Text, Type ())]
        edInstanceKinds         --   :: [Type ()]
        edInstanceTypes         --   :: [Type ()]
        edInstanceConstraints   --   :: Maybe [Constraint ()]
        edInstanceChain         --   :: Maybe ChainId -- contains sourcepos, can we skip it?
        edInstanceChainIndex    --   :: Integer
        edInstanceNameSource    --   :: NameSource
        _ -- (edInstanceSourceSpan      :: SourceSpan
      -> CacheEDInstance
          edInstanceClassName
          edInstanceName
          (fmap (fmap (const ())) <$> edInstanceForAll)
          (fmap (const ()) <$> edInstanceKinds)
          (fmap (const ()) <$> edInstanceTypes)
          (fmap (fmap (const ())) <$> edInstanceConstraints)
          edInstanceChain
          edInstanceChainIndex
          edInstanceNameSource

-- | Generate an externs file for all declarations in a module.
--
-- The `Map Ident Ident` argument should contain any top-level `GenIdent`s that
-- were rewritten to `Ident`s when the module was compiled; this rewrite only
-- happens in the CoreFn, not the original module AST, so it needs to be
-- applied to the exported names here also. (The appropriate map is returned by
-- `L.P.Renamer.renameInModule`.)
moduleToExternsFile :: M.Map ModuleName ExternsFile -> Module -> Environment -> M.Map Ident Ident -> ExternsFile
moduleToExternsFile _ (Module _ _ _ _ Nothing) _ _ = internalError "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile externsMap (Module ss _ mn ds (Just exps)) env renamedIdents = ExternsFile{..}
  where
  efVersion       = T.pack (showVersion Paths.version)
  efModuleName    = mn
  efExports       = map renameRef exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = concat $ map snd $ bcCacheDeclarationsPre
  efSourceSpan    = ss

  efBuildCache = BuildCacheFile efVersion efModuleName bcCacheDeclarations bcCacheImports

  bcCacheDeclarations =
    foldr
      (\(k,vs) m1 ->
        foldr
          (\v m2 ->
            M.insertWith (++) (serialise (declRefToCacheRef k)) [serialise (extDeclToCacheKey v)] m2
          )
          m1
          vs
      )
      M.empty
      bcCacheDeclarationsPre
  bcCacheDeclarationsPre :: [(DeclarationRef, [ExternsDeclaration])]
  bcCacheDeclarationsPre = (\ref -> (ref, (toExternsDeclaration ref))) <$> exps

  bcCacheImports :: M.Map ModuleName (M.Map B.ByteString [B.ByteString])
  bcCacheImports =  -- TODO[drathier]: fill in

  -----

      externsMap
        & M.filterWithKey (\k _ -> elem k importModuleNames)
        & fmap _efBuildCache
        & fmap _bcCacheDeclarations


  importModuleNames = eiModule <$> efImports

  -----

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find ((== Just op) . getValueOpRef) exps)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find ((== Just op) . getTypeOpRef) exps)
  typeFixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration]
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Qualified (Just mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (Just mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData rs) -> [ EDType pn kind (ExternData rs) ]
      Just (kind, tk@(DataType _ _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (Just mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (Just mn) ident `M.lookup` names env
    = [ EDValue (lookupRenamedIdent ident) ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictTypeName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (Just mn) className `M.lookup` typeClasses env
    , Just (kind, tk) <- Qualified (Just mn) (coerceProperName className) `M.lookup` types env
    , Just (dictKind, dictData@(DataType _ _ [(dctor, _)])) <- Qualified (Just mn) dictName `M.lookup` types env
    , Just (dty, _, ty, args) <- Qualified (Just mn) dctor `M.lookup` dataConstructors env
    = [ EDType (coerceProperName className) kind tk
      , EDType dictName dictKind dictData
      , EDDataConstructor dctor dty dictName ty args
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef ss' ident ns)
    = [ EDInstance tcdClassName (lookupRenamedIdent ident) tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex ns ss'
      | m1 <- maybeToList (M.lookup (Just mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (Just mn) ident) m2)
      , TypeClassDictionaryInScope{..} <- NEL.toList nel
      ]
  toExternsDeclaration _ = []

  renameRef :: DeclarationRef -> DeclarationRef
  renameRef = \case
    ValueRef ss' ident -> ValueRef ss' $ lookupRenamedIdent ident
    TypeInstanceRef ss' ident _ | not $ isPlainIdent ident -> TypeInstanceRef ss' (lookupRenamedIdent ident) CompilerNamed
    other -> other

  lookupRenamedIdent :: Ident -> Ident
  lookupRenamedIdent = flip (join M.findWithDefault) renamedIdents

externsFileName :: FilePath
externsFileName = "externs.cbor"
