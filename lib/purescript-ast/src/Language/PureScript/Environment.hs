module Language.PureScript.Environment where

import Prelude.Compat
import Protolude (ordNub)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree, rootLabel)
import qualified Data.Graph as G
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NEL

import Language.PureScript.AST.SourcePos
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import qualified Language.PureScript.Constants.Prim as C

-- | The @Environment@ defines all values and types which are currently in scope:
data Environment = Environment
  { names :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Values currently in scope
  , types :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -- ^ Type names currently in scope
  , dataConstructors :: M.Map (Qualified (ProperName 'ConstructorName)) (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
  -- ^ Data constructors currently in scope, along with their associated type
  -- constructor name, argument types and return type.
  , roleDeclarations :: M.Map (Qualified (ProperName 'TypeName)) [Role]
  -- ^ Explicit role declarations currently in scope. Note that this field is
  -- only used to store declared roles temporarily until they can be checked;
  -- to find a type's real checked and/or inferred roles, refer to the TypeKind
  -- in the `types` field.
  , typeSynonyms :: M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)
  -- ^ Type synonyms currently in scope
  , typeClassDictionaries :: M.Map (Maybe ModuleName) (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
  -- ^ Available type class dictionaries. When looking up 'Nothing' in the
  -- outer map, this returns the map of type class dictionaries in local
  -- scope (ie dictionaries brought in by a constrained type).
  , typeClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
  -- ^ Type classes
  } deriving (Show, Generic)

instance NFData Environment

-- | Information about a type class
data TypeClassData = TypeClassData
  { typeClassArguments :: [(Text, Maybe SourceType)]
  -- ^ A list of type argument names, and their kinds, where kind annotations
  -- were provided.
  , typeClassMembers :: [(Ident, SourceType)]
  -- ^ A list of type class members and their types. Type arguments listed above
  -- are considered bound in these types.
  , typeClassSuperclasses :: [SourceConstraint]
  -- ^ A list of superclasses of this type class. Type arguments listed above
  -- are considered bound in the types appearing in these constraints.
  , typeClassDependencies :: [FunctionalDependency]
  -- ^ A list of functional dependencies for the type arguments of this class.
  , typeClassDeterminedArguments :: S.Set Int
  -- ^ A set of indexes of type argument that are fully determined by other
  -- arguments via functional dependencies. This can be computed from both
  -- typeClassArguments and typeClassDependencies.
  , typeClassCoveringSets :: S.Set (S.Set Int)
  -- ^ A sets of arguments that can be used to infer all other arguments.
  , typeClassIsEmpty :: Bool
  -- ^ Whether or not dictionaries for this type class are necessarily empty.
  } deriving (Show, Generic)

instance NFData TypeClassData

-- | A functional dependency indicates a relationship between two sets of
-- type arguments in a class declaration.
data FunctionalDependency = FunctionalDependency
  { fdDeterminers :: [Int]
  -- ^ the type arguments which determine the determined type arguments
  , fdDetermined  :: [Int]
  -- ^ the determined type arguments
  } deriving (Show, Generic)

instance NFData FunctionalDependency
instance Serialise FunctionalDependency

instance A.FromJSON FunctionalDependency where
  parseJSON = A.withObject "FunctionalDependency" $ \o ->
    FunctionalDependency
      <$> o .: "determiners"
      <*> o .: "determined"

instance A.ToJSON FunctionalDependency where
  toJSON FunctionalDependency{..} =
    A.object [ "determiners" .= fdDeterminers
             , "determined" .= fdDetermined
             ]

-- | The initial environment with no values and only the default javascript types defined
initEnvironment :: Environment
initEnvironment = Environment M.empty allPrimTypes M.empty M.empty M.empty M.empty allPrimClasses

-- | A constructor for TypeClassData that computes which type class arguments are fully determined
-- and argument covering sets.
-- Fully determined means that this argument cannot be used when selecting a type class instance.
-- A covering set is a minimal collection of arguments that can be used to find an instance and
-- therefore determine all other type arguments.
--
-- An example of the difference between determined and fully determined would be with the class:
-- ```class C a b c | a -> b, b -> a, b -> c```
-- In this case, `a` must differ when `b` differs, and vice versa - each is determined by the other.
-- Both `a` and `b` can be used in selecting a type class instance. However, `c` cannot - it is
-- fully determined by `a` and `b`.
--
-- Define a graph of type class arguments with edges being fundep determiners to determined. Each
-- argument also has a self looping edge.
-- An argument is fully determined if doesn't appear at the start of a path of strongly connected components.
-- An argument is not fully determined otherwise.
--
-- The way we compute this is by saying: an argument X is fully determined if there are arguments that
-- determine X that X does not determine. This is the same thing: everything X determines includes everything
-- in its SCC, and everything determining X is either before it in an SCC path, or in the same SCC.
makeTypeClassData
  :: [(Text, Maybe SourceType)]
  -> [(Ident, SourceType)]
  -> [SourceConstraint]
  -> [FunctionalDependency]
  -> Bool
  -> TypeClassData
makeTypeClassData args m s deps tcIsEmpty = TypeClassData args m s deps determinedArgs coveringSets tcIsEmpty
  where
    argumentIndices = [0 .. length args - 1]

    -- each argument determines themselves
    identities = (\i -> (i, [i])) <$> argumentIndices

    -- list all the edges in the graph: for each fundep an edge exists for each determiner to each determined
    contributingDeps = M.fromListWith (++) $ identities ++ do
      fd <- deps
      src <- fdDeterminers fd
      (src, fdDetermined fd) : map (, []) (fdDetermined fd)

    -- build a graph of which arguments determine other arguments
    (depGraph, fromVertex, fromKey) = G.graphFromEdges ((\(n, v) -> (n, n, ordNub v)) <$> M.toList contributingDeps)

    -- do there exist any arguments that contribute to `arg` that `arg` doesn't contribute to
    isFunDepDetermined :: Int -> Bool
    isFunDepDetermined arg = case fromKey arg of
      Nothing -> internalError "Unknown argument index in makeTypeClassData"
      Just v -> let contributesToVar = G.reachable (G.transposeG depGraph) v
                    varContributesTo = G.reachable depGraph v
                in any (\r -> not (r `elem` varContributesTo)) contributesToVar

    -- find all the arguments that are determined
    determinedArgs :: S.Set Int
    determinedArgs = S.fromList $ filter isFunDepDetermined argumentIndices

    argFromVertex :: G.Vertex -> Int
    argFromVertex index = let (_, arg, _) = fromVertex index in arg

    isVertexDetermined :: G.Vertex -> Bool
    isVertexDetermined = isFunDepDetermined . argFromVertex

    -- from an scc find the non-determined args
    sccNonDetermined :: Tree G.Vertex -> Maybe [Int]
    sccNonDetermined tree
      -- if any arg in an scc is determined then all of them are
      | isVertexDetermined (rootLabel tree) = Nothing
      | otherwise = Just (argFromVertex <$> toList tree)

    -- find the covering sets
    coveringSets :: S.Set (S.Set Int)
    coveringSets = let funDepSets = sequence (mapMaybe sccNonDetermined (G.scc depGraph))
                   in S.fromList (S.fromList <$> funDepSets)

-- | The visibility of a name in scope
data NameVisibility
  = Undefined
  -- ^ The name is defined in the current binding group, but is not visible
  | Defined
  -- ^ The name is defined in the another binding group, or has been made visible by a function binder
  deriving (Show, Eq, Generic)

instance NFData NameVisibility
instance Serialise NameVisibility

-- | A flag for whether a name is for an private or public value - only public values will be
-- included in a generated externs file.
data NameKind
  = Private
  -- ^ A private value introduced as an artifact of code generation (class instances, class member
  -- accessors, etc.)
  | Public
  -- ^ A public value for a module member or foreign import declaration
  | External
  -- ^ A name for member introduced by foreign import
  deriving (Show, Eq, Generic)

instance NFData NameKind
instance Serialise NameKind

-- | The kinds of a type
data TypeKind
  = DataType DataDeclType [(Text, Maybe SourceType, Role)] [(ProperName 'ConstructorName, [SourceType])]
  -- ^ Data type
  | TypeSynonym
  -- ^ Type synonym
  | ExternData [Role]
  -- ^ Foreign data
  | LocalTypeVariable
  -- ^ A local type variable
  | ScopedTypeVar
  -- ^ A scoped type variable
  deriving (Show, Eq, Generic)

instance NFData TypeKind
instance Serialise TypeKind

-- | The type ('data' or 'newtype') of a data type declaration
data DataDeclType
  = Data
  -- ^ A standard data constructor
  | Newtype
  -- ^ A newtype constructor
  deriving (Show, Eq, Ord, Generic)

instance NFData DataDeclType
instance Serialise DataDeclType

showDataDeclType :: DataDeclType -> Text
showDataDeclType Data = "data"
showDataDeclType Newtype = "newtype"

instance A.ToJSON DataDeclType where
  toJSON = A.toJSON . showDataDeclType

instance A.FromJSON DataDeclType where
  parseJSON = A.withText "DataDeclType" $ \str ->
    case str of
      "data" -> return Data
      "newtype" -> return Newtype
      other -> fail $ "invalid type: '" ++ T.unpack other ++ "'"

-- | Construct a ProperName in the Prim module
primName :: Text -> Qualified (ProperName a)
primName = Qualified (Just C.Prim) . ProperName

-- | Construct a 'ProperName' in the @Prim.NAME@ module.
primSubName :: Text -> Text -> Qualified (ProperName a)
primSubName sub =
  Qualified (Just $ ModuleName $ C.prim <> "." <> sub) . ProperName

primKind :: Text -> SourceType
primKind = primTy

primSubKind :: Text -> Text -> SourceType
primSubKind sub = TypeConstructor nullSourceAnn . primSubName sub

-- | Kind of ground types
kindType :: SourceType
kindType = primKind C.typ

kindConstraint :: SourceType
kindConstraint = primKind C.constraint

isKindType :: Type a -> Bool
isKindType (TypeConstructor _ n) = n == primName C.typ
isKindType _ = False

kindSymbol :: SourceType
kindSymbol = primKind C.symbol

kindDoc :: SourceType
kindDoc = primSubKind C.typeError C.doc

kindBoolean :: SourceType
kindBoolean = primSubKind C.moduleBoolean C.kindBoolean

kindOrdering :: SourceType
kindOrdering = primSubKind C.moduleOrdering C.kindOrdering

kindRowList :: SourceType -> SourceType
kindRowList = TypeApp nullSourceAnn (primSubKind C.moduleRowList C.kindRowList)

kindRow :: SourceType -> SourceType
kindRow = TypeApp nullSourceAnn (primKind C.row)

kindOfREmpty :: SourceType
kindOfREmpty = tyForall "k" kindType (kindRow (tyVar "k"))

-- | Construct a type in the Prim module
primTy :: Text -> SourceType
primTy = TypeConstructor nullSourceAnn . primName

-- | Type constructor for functions
tyFunction :: SourceType
tyFunction = primTy "Function"

-- | Type constructor for strings
tyString :: SourceType
tyString = primTy "String"

-- | Type constructor for strings
tyChar :: SourceType
tyChar = primTy "Char"

-- | Type constructor for numbers
tyNumber :: SourceType
tyNumber = primTy "Number"

-- | Type constructor for integers
tyInt :: SourceType
tyInt = primTy "Int"

-- | Type constructor for booleans
tyBoolean :: SourceType
tyBoolean = primTy "Boolean"

-- | Type constructor for arrays
tyArray :: SourceType
tyArray = primTy "Array"

-- | Type constructor for records
tyRecord :: SourceType
tyRecord = primTy "Record"

tyVar :: Text -> SourceType
tyVar = TypeVar nullSourceAnn

tyForall :: Text -> SourceType -> SourceType -> SourceType
tyForall var k ty = ForAll nullSourceAnn var (Just k) ty Nothing

-- | Check whether a type is a record
isObject :: Type a -> Bool
isObject = isTypeOrApplied tyRecord

-- | Check whether a type is a function
isFunction :: Type a -> Bool
isFunction = isTypeOrApplied tyFunction

isTypeOrApplied :: Type a -> Type b -> Bool
isTypeOrApplied t1 (TypeApp _ t2 _) = eqType t1 t2
isTypeOrApplied t1 t2 = eqType t1 t2

-- | Smart constructor for function types
function :: SourceType -> SourceType -> SourceType
function t1 t2 = TypeApp nullSourceAnn (TypeApp nullSourceAnn tyFunction t1) t2

-- To make reading the kind signatures below easier
(-:>) :: SourceType -> SourceType -> SourceType
(-:>) = function
infixr 4 -:>

primClass :: Qualified (ProperName 'TypeName) -> (SourceType -> SourceType) -> [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
primClass name mkKind =
  [ let k = mkKind kindConstraint
    in (name, (k, ExternData (nominalRolesForKind k)))
  , let k = mkKind kindType
    in (dictSynonymName <$> name, (k, TypeSynonym))
  ]

-- | The primitive types in the external environment with their
-- associated kinds. There are also pseudo `Fail`, `Warn`, and `Partial` types
-- that correspond to the classes with the same names.
primTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypes =
  M.fromList
    [ (primName "Type",             (kindType, ExternData []))
    , (primName "Constraint",       (kindType, ExternData []))
    , (primName "Symbol",           (kindType, ExternData []))
    , (primName "Row",              (kindType -:> kindType, ExternData [Phantom]))
    , (primName "Function",         (kindType -:> kindType -:> kindType, ExternData [Representational, Representational]))
    , (primName "Array",            (kindType -:> kindType, ExternData [Representational]))
    , (primName "Record",           (kindRow kindType -:> kindType, ExternData [Representational]))
    , (primName "String",           (kindType, ExternData []))
    , (primName "Char",             (kindType, ExternData []))
    , (primName "Number",           (kindType, ExternData []))
    , (primName "Int",              (kindType, ExternData []))
    , (primName "Boolean",          (kindType, ExternData []))
    , (primName "Partial",          (kindConstraint, ExternData []))
    ]

-- | This 'Map' contains all of the prim types from all Prim modules.
allPrimTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
allPrimTypes = M.unions
  [ primTypes
  , primBooleanTypes
  , primCoerceTypes
  , primOrderingTypes
  , primRowTypes
  , primRowListTypes
  , primSymbolTypes
  , primTypeErrorTypes
  ]

primBooleanTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primBooleanTypes =
  M.fromList
    [ (primSubName C.moduleBoolean "True", (tyBoolean, ExternData []))
    , (primSubName C.moduleBoolean "False", (tyBoolean, ExternData []))
    ]

primCoerceTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primCoerceTypes =
  M.fromList $ mconcat
    [ primClass (primSubName C.moduleCoerce "Coercible") (\kind -> tyForall "k" kindType $ tyVar "k" -:> tyVar "k" -:> kind)
    ]

primOrderingTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primOrderingTypes =
  M.fromList
    [ (primSubName C.moduleOrdering "Ordering", (kindType, ExternData []))
    , (primSubName C.moduleOrdering "LT", (kindOrdering, ExternData []))
    , (primSubName C.moduleOrdering "EQ", (kindOrdering, ExternData []))
    , (primSubName C.moduleOrdering "GT", (kindOrdering, ExternData []))
    ]

primRowTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowTypes =
  M.fromList $ mconcat
    [ primClass (primSubName C.moduleRow "Union") (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    , primClass (primSubName C.moduleRow "Nub")   (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    , primClass (primSubName C.moduleRow "Lacks") (\kind -> tyForall "k" kindType $ kindSymbol -:> kindRow (tyVar "k") -:> kind)
    , primClass (primSubName C.moduleRow "Cons")  (\kind -> tyForall "k" kindType $ kindSymbol -:> tyVar "k" -:> kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    ]

primRowListTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowListTypes =
  M.fromList $
    [ (primSubName C.moduleRowList "RowList", (kindType -:> kindType, ExternData [Phantom]))
    , (primSubName C.moduleRowList "Cons", (tyForall "k" kindType $ kindSymbol -:> tyVar "k" -:> kindRowList (tyVar "k") -:> kindRowList (tyVar "k"), ExternData [Phantom, Phantom, Phantom]))
    , (primSubName C.moduleRowList "Nil", (tyForall "k" kindType $ kindRowList (tyVar "k"), ExternData []))
    ] <> mconcat
    [ primClass (primSubName C.moduleRowList "RowToList")  (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRowList (tyVar "k") -:> kind)
    ]

primSymbolTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primSymbolTypes =
  M.fromList $ mconcat
    [ primClass (primSubName C.moduleSymbol "Append")  (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
    , primClass (primSubName C.moduleSymbol "Compare") (\kind -> kindSymbol -:> kindSymbol -:> kindOrdering -:> kind)
    , primClass (primSubName C.moduleSymbol "Cons")    (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
    ]

primTypeErrorTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypeErrorTypes =
  M.fromList $
    [ (primSubName C.typeError "Doc", (kindType, ExternData []))
    , (primSubName C.typeError "Fail", (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (primSubName C.typeError "Warn", (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (primSubName C.typeError "Text", (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (primSubName C.typeError "Quote", (kindType -:> kindDoc, ExternData [Phantom]))
    , (primSubName C.typeError "QuoteLabel", (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (primSubName C.typeError "Beside", (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    , (primSubName C.typeError "Above", (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    ] <> mconcat
    [ primClass (primSubName C.typeError "Fail") (\kind -> kindDoc -:> kind)
    , primClass (primSubName C.typeError "Warn") (\kind -> kindDoc -:> kind)
    ]

-- | The primitive class map. This just contains the `Partial` class.
-- `Partial` is used as a kind of magic constraint for partial functions.
primClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primClasses =
  M.fromList
    [ (primName "Partial", (makeTypeClassData [] [] [] [] True))
    ]

-- | This contains all of the type classes from all Prim modules.
allPrimClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
allPrimClasses = M.unions
  [ primClasses
  , primCoerceClasses
  , primRowClasses
  , primRowListClasses
  , primSymbolClasses
  , primTypeErrorClasses
  ]

primCoerceClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primCoerceClasses =
  M.fromList
    -- class Coercible (a :: k) (b :: k)
    [ (primSubName C.moduleCoerce "Coercible", makeTypeClassData
        [ ("a", Just (tyVar "k"))
        , ("b", Just (tyVar "k"))
        ] [] [] [] True)
    ]

primRowClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primRowClasses =
  M.fromList
    -- class Union (left :: Row k) (right :: Row k) (union :: Row k) | left right -> union, right union -> left, union left -> right
    [ (primSubName C.moduleRow "Union", makeTypeClassData
        [ ("left", Just (kindRow (tyVar "k")))
        , ("right", Just (kindRow (tyVar "k")))
        , ("union", Just (kindRow (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [1, 2] [0]
        , FunctionalDependency [2, 0] [1]
        ] True)

    -- class Nub (original :: Row k) (nubbed :: Row k) | original -> nubbed
    , (primSubName C.moduleRow "Nub", makeTypeClassData
        [ ("original", Just (kindRow (tyVar "k")))
        , ("nubbed", Just (kindRow (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0] [1]
        ] True)

    -- class Lacks (label :: Symbol) (row :: Row k)
    , (primSubName C.moduleRow "Lacks", makeTypeClassData
        [ ("label", Just kindSymbol)
        , ("row", Just (kindRow (tyVar "k")))
        ] [] [] [] True)

    -- class RowCons (label :: Symbol) (a :: k) (tail :: Row k) (row :: Row k) | label tail a -> row, label row -> tail a
    , (primSubName C.moduleRow "Cons", makeTypeClassData
        [ ("label", Just kindSymbol)
        , ("a", Just (tyVar "k"))
        , ("tail", Just (kindRow (tyVar "k")))
        , ("row", Just (kindRow (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0, 1, 2] [3]
        , FunctionalDependency [0, 3] [1, 2]
        ] True)
    ]

primRowListClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primRowListClasses =
  M.fromList
    -- class RowToList (row :: Row k) (list :: RowList k) | row -> list
    [ (primSubName C.moduleRowList "RowToList", makeTypeClassData
        [ ("row", Just (kindRow (tyVar "k")))
        , ("list", Just (kindRowList (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0] [1]
        ] True)
    ]

primSymbolClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primSymbolClasses =
  M.fromList
    -- class Append (left :: Symbol) (right :: Symbol) (appended :: Symbol) | left right -> appended, right appended -> left, appended left -> right
    [ (primSubName C.moduleSymbol "Append", makeTypeClassData
        [ ("left", Just kindSymbol)
        , ("right", Just kindSymbol)
        , ("appended", Just kindSymbol)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [1, 2] [0]
        , FunctionalDependency [2, 0] [1]
        ] True)

    -- class Compare (left :: Symbol) (right :: Symbol) (ordering :: Ordering) | left right -> ordering
    , (primSubName C.moduleSymbol "Compare", makeTypeClassData
        [ ("left", Just kindSymbol)
        , ("right", Just kindSymbol)
        , ("ordering", Just kindOrdering)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        ] True)

    -- class Cons (head :: Symbol) (tail :: Symbol) (symbol :: Symbol) | head tail -> symbol, symbol -> head tail
    , (primSubName C.moduleSymbol "Cons", makeTypeClassData
        [ ("head", Just kindSymbol)
        , ("tail", Just kindSymbol)
        , ("symbol", Just kindSymbol)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [2] [0, 1]
        ] True)
    ]

primTypeErrorClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primTypeErrorClasses =
  M.fromList
    -- class Fail (message :: Symbol)
    [ (primSubName C.typeError "Fail", makeTypeClassData
        [("message", Just kindDoc)] [] [] [] True)

    -- class Warn (message :: Symbol)
    , (primSubName C.typeError "Warn", makeTypeClassData
        [("message", Just kindDoc)] [] [] [] True)
    ]

-- | Finds information about data constructors from the current environment.
lookupConstructor :: Environment -> Qualified (ProperName 'ConstructorName) -> (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
lookupConstructor env ctor =
  fromMaybe (internalError "Data constructor not found") $ ctor `M.lookup` dataConstructors env

-- | Checks whether a data constructor is for a newtype.
isNewtypeConstructor :: Environment -> Qualified (ProperName 'ConstructorName) -> Bool
isNewtypeConstructor e ctor = case lookupConstructor e ctor of
  (Newtype, _, _, _) -> True
  (Data, _, _, _) -> False

-- | Finds information about values from the current environment.
lookupValue :: Environment -> Qualified Ident -> Maybe (SourceType, NameKind, NameVisibility)
lookupValue env ident = ident `M.lookup` names env

dictSynonymName' :: Text -> Text
dictSynonymName' = (<> "$Dict")

dictSynonymName :: ProperName a -> ProperName a
dictSynonymName = ProperName . dictSynonymName' . runProperName

isDictSynonym :: ProperName a -> Bool
isDictSynonym = T.isSuffixOf "$Dict" . runProperName

-- |
-- Given the kind of a type, generate a list @Nominal@ roles. This is used for
-- opaque foreign types as well as type classes.
nominalRolesForKind :: Type a -> [Role]
nominalRolesForKind k = replicate (kindArity k) Nominal

kindArity :: Type a -> Int
kindArity = length . fst . unapplyKinds

unapplyKinds :: Type a -> ([Type a], Type a)
unapplyKinds = go [] where
  go kinds (TypeApp _ (TypeApp _ fn k1) k2)
    | eqType fn tyFunction = go (k1 : kinds) k2
  go kinds (ForAll _ _ _ k _) = go kinds k
  go kinds k = (reverse kinds, k)
