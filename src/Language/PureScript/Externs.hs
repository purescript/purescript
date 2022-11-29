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

import Prelude

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
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Control.Monad.State.Lazy (State, runState, modify)
import Debug.Trace
import PrettyPrint

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
  -- NOTE[drathier]: using bytestrings here for much faster encoding/decoding, since we only care about equality for now. When we want to look at what's actually imported, we have to split this up a bit; BS into Map BS BS.
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
  { bcVersion :: Text
  -- ^ The externs version
  , bcModuleName :: ModuleName
  -- ^ Module name
  -- , bcCacheDeclarations :: M.Map DeclarationCacheRef [ExternCacheKey]
  , bcCacheBlob :: B.ByteString
  -- ^ All of the things, in one ByteString. We only care about equality anyway.
  , bcCacheDecls :: M.Map DeclarationRef B.ByteString
  -- ^ Exported things which we might want to re-export in modules depending on this one
  , bcDeclarations :: M.Map Text ([(Text, Maybe (Type ()))], Type ())
  -- ^ Exported things which we might want to re-export in modules depending on this one
  , bcDeclShapes :: M.Map (ProperName 'TypeName) (CacheShape, CacheTypeDetails)
  -- ^ WIP
  , bcCacheDeps :: M.Map ModuleName B.ByteString
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
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (ByModuleName efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx ns ss) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (ByModuleName efModuleName) (typeClassDictionaries env) }
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
  qual = Qualified (ByModuleName efModuleName)


_efBuildCache :: ExternsFile -> BuildCacheFile
_efBuildCache = efBuildCache

_bcCacheBlob :: BuildCacheFile -> B.ByteString
_bcCacheBlob = bcCacheBlob

_bcDeclShapes :: BuildCacheFile -> M.Map (ProperName 'TypeName) (CacheShape, CacheTypeDetails)
_bcDeclShapes = bcDeclShapes

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
      -- , cacheEdTypeDeclarationKind     :: TypeKind -- TODO[drathier]: contains SourceType for adt's, can we safely skip the entire field? Probably not
      }
  -- | A type synonym
  | CacheEDTypeSynonym
      { cacheEdTypeSynonymName         :: ProperName 'TypeName
      , cacheEdTypeSynonymArguments    :: [(Text, Maybe (Type ()))]
      , cacheEdTypeSynonymType         :: Type () -- CacheType
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

extDeclToCacheKey :: M.Map Text ([(Text, Maybe (Type ()))], Type ()) -> ExternsDeclaration -> ExternCacheKey
extDeclToCacheKey _decls = \case
    EDType
      edTypeName             --   :: ProperName 'TypeName
      edTypeKind             --   :: Type ()
      _ -- (edTypeDeclarationKind     :: TypeKind) -- contains SourceType for adt's, can we safely skip the entire field?
       ->
        CacheEDType
          edTypeName
          (const () <$> edTypeKind)
    -- A type synonym
    EDTypeSynonym
        edTypeSynonymName       --  :: ProperName 'TypeName
        edTypeSynonymArguments  --  :: [(Text, Maybe (Type ()))]
        edTypeSynonymType       --  :: Type ()
      ->
        CacheEDTypeSynonym
          edTypeSynonymName
          (fmap (fmap (fmap (const ()))) <$> edTypeSynonymArguments)
          (const () <$> edTypeSynonymType)
    -- A data constructor
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
    -- A value declaration
    EDValue
        edValueName        --  :: Ident
        edValueType        --  :: Type ()
      -> CacheEDValue
          edValueName
          (const () <$> edValueType)
    -- A type class declaration
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
    -- An instance declaration
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

  -- TODO[drathier]: look up relevant defs in `ds` when exposing type aliases (and presumably type classes too?), and add them to the externs file for easy diffing

  ------ decls

  ------

  bcDeclarations :: M.Map Text ([(Text, Maybe (Type ()))], Type ())
  bcDeclarations = M.fromList $ concatMap typeDeclForCache ds
  efBuildCache = BuildCacheFile efVersion efModuleName bcCacheBlob bcCacheDecls bcDeclarations bcDeclShapes bcCacheImports

  bcReExportDeclShapes :: M.Map (ProperName 'TypeName) (CacheShape, CacheTypeDetails)
  bcReExportDeclShapes =
    -- ASSUMPTION[drathier]: no exposed constructors? then other modules cannot possibly care about the internal shape of this data type, since cross-module inlining isn't a thing
    let
      getModu modu =
        externsMap
          & M.lookup modu
          & fromMaybe (trace ("bcReExportDeclShapes: missing module in externsMap:" <> sShow (mn, modu, M.keys externsMap)) $ internalError "bcReExportDeclShapes: missing module in externsMap")
          & _efBuildCache
          & _bcDeclShapes

      f (TypeClassRef _ _) = []
      f (TypeOpRef _ _) = []
      f (TypeRef _ _ _) = []
      f (ValueRef _ _) = []
      f (ValueOpRef _ _) = []
      f (TypeInstanceRef _ _ _) = []
      f (ModuleRef _ _) = [] -- Anything re-exported via a ModuleRef is also exposed on a per-decl basis via an ReExportRef, so we only use ReExportRef to find re-exports.
      f (ReExportRef _ (ExportSource _ (ModuleName moduName)) _) | "Prim" `T.isPrefixOf` moduName = []
      f (ReExportRef _ (ExportSource _ mn2) (TypeRef _ tn _)) =
        getModu mn2
        & (\m -> M.intersection m (M.singleton tn ()))
        & M.toList
      f (ReExportRef _ _ _) = []
    in
    M.fromList $ concatMap f exps

  expsTypeNames :: M.Map (ProperName 'TypeName) Bool
  expsTypeNames =
    -- ASSUMPTION[drathier]: no exposed constructors? then other modules cannot possibly care about the internal shape of this data type, since cross-module inlining isn't a thing
    let
      f (TypeClassRef _ _) = []
      f (TypeOpRef _ _) = []
      -- type synonyms don't have ctors in ast but do effectively have a single exposed ctor, so keep it in
      f (TypeRef _ tn _) | (Just (_, TypeSynonym)) <- Qualified (ByModuleName mn) tn `M.lookup` types env = [(tn, True)]
      -- data types with no public ctors are opaque to all other modules, so no need to expose its internal shapes
      f (TypeRef _ tn (Just [])) = [(tn, False)]
      -- if there are any exposed ctors, expose the type shape
      f (TypeRef _ tn _) = [(tn, True)]
      f (ValueRef _ _) = []
      f (ValueOpRef _ _) = []
      f (TypeInstanceRef _ _ _) = []
      -- re-exports are handled elsewhere
      f (ModuleRef _ _) = []
      f (ReExportRef _ _ _) = []
    in
    M.fromList $ concatMap f exps

  bcDeclShapes :: M.Map (ProperName 'TypeName) (CacheShape, CacheTypeDetails)
  bcDeclShapes =
    M.intersectionWith
      (\(cs, ctd) shouldShowInternals ->
        if shouldShowInternals then
          (cs, ctd)
        else
          (cs, CacheTypeDetails mempty)
      )
      bcDeclShapesAll
      expsTypeNames
    -- add in re-exports
    & (<>) bcReExportDeclShapes
    -- & (\v -> trace ("bcDeclShapes:" <> sShow (mn, exps, v)) v)


  bcDeclShapesAll :: M.Map (ProperName 'TypeName) (CacheShape, CacheTypeDetails)
  bcDeclShapesAll =
    ds
    & concatMap declToCacheShape
    & M.fromList
    & M.map (\(cs, cts) ->
      ( cs
      , cts
        & M.mapWithKey (\k () ->
          case k of
            Qualified (BySourcePos _) _ ->
              internalError "dsCacheShapesWithDetails: unexpected Qualified BySourcePos"

            Qualified (ByModuleName km@(ModuleName kmn)) tn | "Prim" `T.isPrefixOf` kmn -> (PrimType km tn, CacheTypeDetails mempty)
            Qualified (ByModuleName km) tn | "$" `T.isInfixOf` runProperName tn -> (TypeClassDictType km tn, CacheTypeDetails mempty)
            Qualified (ByModuleName km) tn | km == mn -> (OwnModuleRef km tn, CacheTypeDetails mempty)
            Qualified (ByModuleName km) tn ->
              let
                moduExterns =
                  M.lookup km externsMap
                    & fromMaybe (trace ("dsCacheShapesWithDetails: missing module in externsMap:" <> sShow (km, tn, M.keys externsMap)) $ internalError "dsCacheShapesWithDetails: missing module in externsMap")
                    & _efBuildCache
                    & _bcDeclShapes
              in
              moduExterns
                & M.lookup tn
                & fromMaybe (trace ("dsCacheShapesWithDetails: missing type in externsMap:" <> sShow (mn, km, tn, M.keys externsMap, moduExterns)) $ internalError "dsCacheShapesWithDetails: missing type in externsMap")
        )
        & M.toList
        & mapMaybe (\case
          (Qualified (ByModuleName km) k, v) -> Just ((km,k), v)
          (Qualified (BySourcePos pos) k, v) -> trace (show ("BcDeclShapesAll-Externs: skipping Qualified BySourcePos" :: String, pos, "k" :: String, k, "v" :: String, v)) Nothing
          ) -- TODO partial
        & M.fromList
        & CacheTypeDetails
      )
    )

  bcCacheDecls :: M.Map DeclarationRef B.ByteString
  bcCacheBlob :: B.ByteString
  (bcCacheDecls, bcCacheBlob) =
    let
      bshow a = BLU.fromString ("[" <> show a <> "]")

      _ = (serialise :: Int -> B.ByteString)

      foldCache :: Show a => Serialise a => [a] -> B.ByteString
      foldCache = foldr (\a acc -> bshow a <> "|" <> acc) B.empty

      cacheDecls :: M.Map DeclarationRef B.ByteString
      cacheDecls =
        M.fromList $ fmap (\(k,v) -> (k, foldCache (extDeclToCacheKey bcDeclarations <$> v))) $ filter (\(k, _) -> elem k efExports) bcCacheDeclarationsPre

      -- cacheDecls2 =
      --   foldr
      --     (\(k,vs) m1 ->
      --       case elem k efExports of
      --         False -> m1
      --         True ->
      --           foldr
      --             (\v acc -> bshow (declRefToCacheRef k) <> ":" <> bshow (extDeclToCacheKey v) <> acc)
      --             m1
      --             vs
      --     )
      --     B.empty
      --     bcCacheDeclarationsPre

      cacheExports = foldCache (declRefToCacheRef <$> efExports)
      cacheImports = foldr (<>) B.empty $ (\v -> (serialise (eiModule v) <> ":" <> removeSourceSpansFromImport (eiImportType v))) <$> efImports
      cacheFixities = foldCache efFixities
      cacheTypeFixities = foldCache efTypeFixities

      removeSourceSpansFromImport = \case
        Implicit -> "Implicit"
        Explicit declRefs -> "Explicit" <> foldCache (declRefToCacheRef <$> declRefs)
        Hiding declRefs -> "Hiding" <> foldCache (declRefToCacheRef <$> declRefs)
    in
      ( cacheDecls
      , cacheExports
      <> cacheImports
      <> cacheFixities
      <> cacheTypeFixities
      )

  bcCacheDeclarationsPre :: [(DeclarationRef, [ExternsDeclaration])]
  bcCacheDeclarationsPre = (\ref -> (ref, (toExternsDeclaration ref))) <$> exps

  bcCacheImports :: M.Map ModuleName B.ByteString
  bcCacheImports =  -- TODO[drathier]: fill in
    externsMap
      & M.filterWithKey (\k _ -> elem k importModuleNames)
      & fmap _efBuildCache
      & fmap _bcCacheBlob

  importModuleNames = eiModule <$> efImports

  typeDeclForCache :: Declaration -> [(Text ,([(Text, Maybe (Type ()))] , Type ()))]
  typeDeclForCache (TypeSynonymDeclaration _ typeName targs underlyingType) =
    [(runProperName typeName, (fmap (fmap (fmap (const ()))) <$> targs, const () <$> underlyingType))]
  typeDeclForCache _ = []

  declToCacheShape
    :: Declaration
    -> [(ProperName 'TypeName, (CacheShape, CacheTypeState))]
  declToCacheShape decl =
    let
      (things, cts) = runState (declToCacheShapeImpl decl) mempty
      f (name, cs) = (name, (cs, cts))
    in f <$> things

  declToCacheShapeImpl
    :: Declaration
    -> State CacheTypeState [(ProperName 'TypeName, CacheShape)]
  declToCacheShapeImpl (DataDeclaration _ dataOrNewtype typeName targs ctors) = do
    let
      handleCtor (DataConstructorDeclaration _ ctorName ctorFields) = do
        ctorFieldsv <- mapM (mapM typeToCacheTypeImpl) ctorFields
        pure (ctorName, ctorFieldsv)

    targsv <- mapM (mapM (mapM typeToCacheTypeImpl)) targs
    ctorsv <- mapM handleCtor ctors

    pure
      [ ( typeName
        , CacheShapeDataDecl
          dataOrNewtype
          typeName
          targsv
          ctorsv
        )
      ]
  -- TODO[drathier]: how do we handle mutually recursive data types?
  declToCacheShapeImpl (DataBindingGroupDeclaration things) = do
    -- e.g. the Void type lives here
    thingsv <- mapM declToCacheShapeImpl things
    pure $ concat thingsv

  declToCacheShapeImpl (ExternDataDeclaration _ typeName underlyingType) = do
    underlyingTypev <- typeToCacheTypeImpl underlyingType
    pure
      [ ( typeName
        , CacheShapeForeignTypeDecl
          typeName
          underlyingTypev
        )
      ]
  declToCacheShapeImpl (TypeSynonymDeclaration _ typeName targs underlyingType) = do
    targsv <- mapM (mapM (mapM typeToCacheTypeImpl)) targs
    underlyingTypev <- typeToCacheTypeImpl underlyingType
    pure
      [ ( typeName
        , CacheShapeTypeDecl
          typeName
          targsv
          underlyingTypev
        )
      ]
  declToCacheShapeImpl _ = pure []

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
    case Qualified (ByModuleName mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (ByModuleName mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData rs) -> [ EDType pn kind (ExternData rs) ]
      Just (kind, tk@(DataType _ _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (ByModuleName mn) ident `M.lookup` names env
    = [ EDValue (lookupRenamedIdent ident) ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictTypeName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (ByModuleName mn) className `M.lookup` typeClasses env
    , Just (kind, tk) <- Qualified (ByModuleName mn) (coerceProperName className) `M.lookup` types env
    , Just (dictKind, dictData@(DataType _ _ [(dctor, _)])) <- Qualified (ByModuleName mn) dictName `M.lookup` types env
    , Just (dty, _, ty, args) <- Qualified (ByModuleName mn) dctor `M.lookup` dataConstructors env
    = [ EDType (coerceProperName className) kind tk
      , EDType dictName dictKind dictData
      , EDDataConstructor dctor dty dictName ty args
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef ss' ident ns)
    = [ EDInstance tcdClassName (lookupRenamedIdent ident) tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex ns ss'
      | m1 <- maybeToList (M.lookup (ByModuleName mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (ByModuleName mn) ident) m2)
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

data CacheShape
  = PrimType ModuleName (ProperName 'TypeName)
  | TypeClassDictType ModuleName (ProperName 'TypeName)
  | OwnModuleRef ModuleName (ProperName 'TypeName)
  | CacheShapeTypeDecl
    (ProperName 'TypeName)
    [(Text, Maybe (Type ()))]
    (Type ())
  | CacheShapeForeignTypeDecl
    (ProperName 'TypeName)
    (Type ())
  | CacheShapeDataDecl
    DataDeclType
    (ProperName 'TypeName)
    [(Text, Maybe (Type ()))]
    [(ProperName 'ConstructorName, [(Ident, Type ())])]
  -- CacheShapeDataRecDecl
  --   DataDeclType
  --   (ProperName 'TypeName)
  --   [(Text, Maybe (Type ()))]
  --   [(ProperName 'ConstructorName, [(Ident, Type ())])]
  deriving (Show, Eq, Ord, Generic)

instance Serialise CacheShape

data CacheTypeDetails = CacheTypeDetails (M.Map (ModuleName, ProperName 'TypeName) (CacheShape, CacheTypeDetails))
  deriving (Show, Eq, Generic)

instance Serialise CacheTypeDetails






----------------


type CacheTypeState = M.Map (Qualified (ProperName 'TypeName)) ()

typeToCacheTypeImpl :: Type a -> State CacheTypeState (Type ())
typeToCacheTypeImpl t = case t of
  TUnknown _ a -> pure $ TUnknown () a
  TypeVar _ a -> pure $ TypeVar () a
  TypeLevelString _ a -> pure $ TypeLevelString () a
  TypeLevelInt _ a -> pure $ TypeLevelInt () a
  TypeWildcard _ a -> pure $ TypeWildcard () a
  TypeConstructor _ a -> do
    modify (M.insert a ())
    pure $ TypeConstructor () a
  TypeOp _ a -> pure $ TypeOp () a
  TypeApp _ a b -> do
    av <- typeToCacheTypeImpl a
    bv <- typeToCacheTypeImpl b
    pure $ TypeApp () av bv
  KindApp _ a b -> do
    av <- typeToCacheTypeImpl a
    bv <- typeToCacheTypeImpl b
    pure $ KindApp () av bv
  ForAll _ a b c d -> do
    bv <- mapM typeToCacheTypeImpl b
    cv <- typeToCacheTypeImpl c
    pure $ ForAll () a bv cv d
  ConstrainedType _ a b -> do
    av <- constraintToCacheTypeImpl a
    bv <- typeToCacheTypeImpl b
    pure $ ConstrainedType () av bv
  Skolem _ a b c d -> do
    bv <- mapM typeToCacheTypeImpl b
    pure $ Skolem () a bv c d
  REmpty _ -> pure $ REmpty ()
  RCons _ a b c -> do
    bv <- typeToCacheTypeImpl b
    cv <- typeToCacheTypeImpl c
    pure $ RCons () a bv cv
  KindedType _ a b -> do
    av <- typeToCacheTypeImpl a
    bv <- typeToCacheTypeImpl b
    pure $ KindedType () av bv
  BinaryNoParensType _ a b c -> do
    av <- typeToCacheTypeImpl a
    bv <- typeToCacheTypeImpl b
    cv <- typeToCacheTypeImpl c
    pure $ BinaryNoParensType () av bv cv
  ParensInType _ a -> do
    av <- typeToCacheTypeImpl a
    pure $ ParensInType () av

constraintToCacheTypeImpl :: Constraint a -> State CacheTypeState (Constraint ())
constraintToCacheTypeImpl (Constraint {..}) = do
  cka <- mapM typeToCacheTypeImpl constraintKindArgs
  ca <- mapM typeToCacheTypeImpl constraintArgs
  pure $ Constraint
    ()
    constraintClass
    cka
    ca
    constraintData
