module Language.PureScript.Environment where

import Prelude

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Codec.Serialise (Serialise)
import Data.Aeson ((.=), (.:), ToJSON, FromJSON)
import Data.Aeson qualified as A
import Data.Foldable (find, fold)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NEL

import Language.PureScript.AST.SourcePos (nullSourceAnn)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Names (Ident, ProperName(..), ProperNameType(..), Qualified, QualifiedBy, coerceProperName)
import Language.PureScript.Roles (Role(..))
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.Types (SourceConstraint, SourceType, Type(..), TypeVarVisibility(..), eqType, srcTypeConstructor, freeTypeVariables)
import Language.PureScript.Constants.Prim qualified as C

-- | The @Environment@ defines all values and types which are currently in scope:
data Environment = Environment
  { names :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Values currently in scope
  , types :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
  -- ^ Type names currently in scope
  , dataConstructors :: M.Map (Qualified (ProperName 'ConstructorName)) (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
  -- ^ Data constructors currently in scope, along with their associated type
  -- constructor name, argument types and return type.
  , typeSynonyms :: M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)
  -- ^ Type synonyms currently in scope
  , typeClassDictionaries :: M.Map QualifiedBy (M.Map (Qualified (ProperName 'ClassName)) (M.Map (Qualified Ident) (NEL.NonEmpty NamedDict)))
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
  , typeClassMembers :: [(Ident, SourceType, Maybe (S.Set (NEL.NonEmpty Int)))]
  -- ^ A list of type class members and their types and whether or not
  -- they have type variables that must be defined using Visible Type Applications.
  -- Type arguments listed above are considered bound in these types.
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
  } deriving (Eq, Show, Generic)

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
initEnvironment = Environment M.empty allPrimTypes M.empty M.empty M.empty allPrimClasses

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
makeTypeClassData args m s deps = TypeClassData args m' s deps determinedArgs coveringSets
  where
    ( determinedArgs, coveringSets ) = computeCoveringSets (length args) deps

    coveringSets' = S.toList coveringSets

    m' = map (\(a, b) -> (a, b, addVtaInfo b)) m
    
    addVtaInfo :: SourceType -> Maybe (S.Set (NEL.NonEmpty Int))
    addVtaInfo memberTy = do
      let mentionedArgIndexes = S.fromList (mapMaybe argToIndex $ freeTypeVariables memberTy)
      let leftovers = map (`S.difference` mentionedArgIndexes) coveringSets'
      S.fromList <$> traverse (NEL.nonEmpty . S.toList) leftovers

    argToIndex :: Text -> Maybe Int
    argToIndex = flip M.lookup $ M.fromList (zipWith ((,) . fst) args [0..])

-- A moving frontier of sets to consider, along with the fundeps that can be
-- applied in each case. At each stage, all sets in the frontier will be the
-- same size, decreasing by 1 each time.
type Frontier = M.Map IS.IntSet (First (IM.IntMap (NEL.NonEmpty IS.IntSet)))
--                         ^                 ^          ^          ^
--         when *these* parameters           |          |          |
--         are still needed,                 |          |          |
--                              *these* parameters      |          |
--                              can be determined       |          |
--                                         from a non-zero         |
--                                         number of fundeps,      |
--                                                      which accept *these*
--                                                      parameters as inputs.

computeCoveringSets :: Int -> [FunctionalDependency] -> (S.Set Int, S.Set (S.Set Int))
computeCoveringSets nargs deps = ( determinedArgs, coveringSets )
  where
    argumentIndices = S.fromList [0 .. nargs - 1]

    -- Compute all sets of arguments that determine the remaining arguments via
    -- functional dependencies. This is done in stages, where each stage
    -- considers sets of the same size to share work.
    allCoveringSets :: S.Set (S.Set Int)
    allCoveringSets = S.map (S.fromDistinctAscList . IS.toAscList) $ fst $ search $
      -- The initial frontier consists of just the set of all parameters and all
      -- fundeps organized into the map structure.
      M.singleton
        (IS.fromList [0 .. nargs - 1]) $
          First $ IM.fromListWith (<>) $ do
            fd <- deps
            let srcs = pure (IS.fromList (fdDeterminers fd))
            tgt <- fdDetermined fd
            pure (tgt, srcs)

      where

      -- Recursively advance the frontier until all frontiers are exhausted
      -- and coverings sets found. The covering sets found during the process
      -- are locally-minimal, in that none can be reduced by a fundep, but
      -- there may be subsets found from other frontiers.
      search :: Frontier -> (S.Set IS.IntSet, ())
      search frontier = unless (null frontier) $ M.foldMapWithKey step frontier >>= search

      -- The input set from the frontier is known to cover all parameters, but
      -- it may be able to be reduced by more fundeps.
      step :: IS.IntSet -> First (IM.IntMap (NEL.NonEmpty IS.IntSet)) -> (S.Set IS.IntSet, Frontier)
      step needed (First inEdges)
        -- If there are no applicable fundeps, record it as a locally minimal
        -- covering set. This has already been reduced to only applicable fundeps
        | IM.null inEdges = (S.singleton needed, M.empty)
        | otherwise       = (S.empty, foldMap removeParameter paramsToTry)

          where

          determined = IM.keys inEdges
          -- If there is an acyclically determined functional dependency, prefer
          -- it to reduce the number of cases to check. That is a dependency
          -- that does not help determine other parameters.
          acycDetermined = find (`IS.notMember` (IS.unions $ concatMap NEL.toList $ IM.elems inEdges)) determined
          paramsToTry = maybe determined pure acycDetermined

          -- For each parameter to be removed to build the next frontier,
          -- delete the fundeps that determine it and filter out the fundeps
          -- that make use of it. Of course, if it an acyclic fundep we already
          -- found that there are none that use it.
          removeParameter :: Int -> Frontier
          removeParameter y =
            M.singleton
              (IS.delete y needed) $
                case acycDetermined of
                  Just _ -> First $ IM.delete y inEdges
                  Nothing ->
                    First $ IM.mapMaybe (NEL.nonEmpty . NEL.filter (y `IS.notMember`)) $ IM.delete y inEdges

    -- Reduce to the inclusion-minimal sets
    coveringSets = S.filter (\v -> not (any (\c -> c `S.isProperSubsetOf` v) allCoveringSets)) allCoveringSets

    -- An argument is determined if it is in no covering set
    determinedArgs = argumentIndices `S.difference` fold coveringSets

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

instance ToJSON TypeKind
instance FromJSON TypeKind

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
  parseJSON = A.withText "DataDeclType" $ \case
    "data" -> return Data
    "newtype" -> return Newtype
    other -> fail $ "invalid type: '" ++ T.unpack other ++ "'"

-- | Kind of ground types
kindType :: SourceType
kindType = srcTypeConstructor C.Type

kindConstraint :: SourceType
kindConstraint = srcTypeConstructor C.Constraint

kindSymbol :: SourceType
kindSymbol = srcTypeConstructor C.Symbol

kindDoc :: SourceType
kindDoc = srcTypeConstructor C.Doc

kindOrdering :: SourceType
kindOrdering = srcTypeConstructor C.TypeOrdering

kindRowList :: SourceType -> SourceType
kindRowList = TypeApp nullSourceAnn (srcTypeConstructor C.RowList)

kindRow :: SourceType -> SourceType
kindRow = TypeApp nullSourceAnn (srcTypeConstructor C.Row)

kindOfREmpty :: SourceType
kindOfREmpty = tyForall "k" kindType (kindRow (tyVar "k"))

-- | Type constructor for functions
tyFunction :: SourceType
tyFunction = srcTypeConstructor C.Function

-- | Type constructor for strings
tyString :: SourceType
tyString = srcTypeConstructor C.String

-- | Type constructor for strings
tyChar :: SourceType
tyChar = srcTypeConstructor C.Char

-- | Type constructor for numbers
tyNumber :: SourceType
tyNumber = srcTypeConstructor C.Number

-- | Type constructor for integers
tyInt :: SourceType
tyInt = srcTypeConstructor C.Int

-- | Type constructor for booleans
tyBoolean :: SourceType
tyBoolean = srcTypeConstructor C.Boolean

-- | Type constructor for arrays
tyArray :: SourceType
tyArray = srcTypeConstructor C.Array

-- | Type constructor for records
tyRecord :: SourceType
tyRecord = srcTypeConstructor C.Record

tyVar :: Text -> SourceType
tyVar = TypeVar nullSourceAnn

tyForall :: Text -> SourceType -> SourceType -> SourceType
tyForall var k ty = ForAll nullSourceAnn TypeVarInvisible var (Just k) ty Nothing

-- | Smart constructor for function types
function :: SourceType -> SourceType -> SourceType
function = TypeApp nullSourceAnn . TypeApp nullSourceAnn tyFunction

-- To make reading the kind signatures below easier
(-:>) :: SourceType -> SourceType -> SourceType
(-:>) = function
infixr 4 -:>

primClass :: Qualified (ProperName 'ClassName) -> (SourceType -> SourceType) -> [(Qualified (ProperName 'TypeName), (SourceType, TypeKind))]
primClass name mkKind =
  [ let k = mkKind kindConstraint
    in (coerceProperName <$> name, (k, ExternData (nominalRolesForKind k)))
  , let k = mkKind kindType
    in (dictTypeName . coerceProperName <$> name, (k, TypeSynonym))
  ]

-- | The primitive types in the external environment with their
-- associated kinds. There are also pseudo `Fail`, `Warn`, and `Partial` types
-- that correspond to the classes with the same names.
primTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypes =
  M.fromList
    [ (C.Type,                         (kindType, ExternData []))
    , (C.Constraint,                   (kindType, ExternData []))
    , (C.Symbol,                       (kindType, ExternData []))
    , (C.Row,                          (kindType -:> kindType, ExternData [Phantom]))
    , (C.Function,                     (kindType -:> kindType -:> kindType, ExternData [Representational, Representational]))
    , (C.Array,                        (kindType -:> kindType, ExternData [Representational]))
    , (C.Record,                       (kindRow kindType -:> kindType, ExternData [Representational]))
    , (C.String,                       (kindType, ExternData []))
    , (C.Char,                         (kindType, ExternData []))
    , (C.Number,                       (kindType, ExternData []))
    , (C.Int,                          (kindType, ExternData []))
    , (C.Boolean,                      (kindType, ExternData []))
    , (C.Partial <&> coerceProperName, (kindConstraint, ExternData []))
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
  , primIntTypes
  , primTypeErrorTypes
  ]

primBooleanTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primBooleanTypes =
  M.fromList
    [ (C.True, (tyBoolean, ExternData []))
    , (C.False, (tyBoolean, ExternData []))
    ]

primCoerceTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primCoerceTypes =
  M.fromList $ mconcat
    [ primClass C.Coercible (\kind -> tyForall "k" kindType $ tyVar "k" -:> tyVar "k" -:> kind)
    ]

primOrderingTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primOrderingTypes =
  M.fromList
    [ (C.TypeOrdering, (kindType, ExternData []))
    , (C.LT, (kindOrdering, ExternData []))
    , (C.EQ, (kindOrdering, ExternData []))
    , (C.GT, (kindOrdering, ExternData []))
    ]

primRowTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowTypes =
  M.fromList $ mconcat
    [ primClass C.RowUnion (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    , primClass C.RowNub   (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    , primClass C.RowLacks (\kind -> tyForall "k" kindType $ kindSymbol -:> kindRow (tyVar "k") -:> kind)
    , primClass C.RowCons  (\kind -> tyForall "k" kindType $ kindSymbol -:> tyVar "k" -:> kindRow (tyVar "k") -:> kindRow (tyVar "k") -:> kind)
    ]

primRowListTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primRowListTypes =
  M.fromList $
    [ (C.RowList, (kindType -:> kindType, ExternData [Phantom]))
    , (C.RowListCons, (tyForall "k" kindType $ kindSymbol -:> tyVar "k" -:> kindRowList (tyVar "k") -:> kindRowList (tyVar "k"), ExternData [Phantom, Phantom, Phantom]))
    , (C.RowListNil, (tyForall "k" kindType $ kindRowList (tyVar "k"), ExternData []))
    ] <> mconcat
    [ primClass C.RowToList  (\kind -> tyForall "k" kindType $ kindRow (tyVar "k") -:> kindRowList (tyVar "k") -:> kind)
    ]

primSymbolTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primSymbolTypes =
  M.fromList $ mconcat
    [ primClass C.SymbolAppend  (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
    , primClass C.SymbolCompare (\kind -> kindSymbol -:> kindSymbol -:> kindOrdering -:> kind)
    , primClass C.SymbolCons    (\kind -> kindSymbol -:> kindSymbol -:> kindSymbol -:> kind)
    ]

primIntTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primIntTypes =
  M.fromList $ mconcat
    [ primClass C.IntAdd      (\kind -> tyInt -:> tyInt -:> tyInt -:> kind)
    , primClass C.IntCompare  (\kind -> tyInt -:> tyInt -:> kindOrdering -:> kind)
    , primClass C.IntMul      (\kind -> tyInt -:> tyInt -:> tyInt -:> kind)
    , primClass C.IntToString (\kind -> tyInt -:> kindSymbol -:> kind)
    ]

primTypeErrorTypes :: M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)
primTypeErrorTypes =
  M.fromList $
    [ (C.Doc, (kindType, ExternData []))
    , (C.Fail <&> coerceProperName, (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (C.Warn <&> coerceProperName, (kindDoc -:> kindConstraint, ExternData [Nominal]))
    , (C.Text, (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (C.Quote, (tyForall "k" kindType $ tyVar "k" -:> kindDoc, ExternData [Phantom]))
    , (C.QuoteLabel, (kindSymbol -:> kindDoc, ExternData [Phantom]))
    , (C.Beside, (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    , (C.Above, (kindDoc -:> kindDoc -:> kindDoc, ExternData [Phantom, Phantom]))
    ] <> mconcat
    [ primClass C.Fail (\kind -> kindDoc -:> kind)
    , primClass C.Warn (\kind -> kindDoc -:> kind)
    ]

-- | The primitive class map. This just contains the `Partial` class.
-- `Partial` is used as a kind of magic constraint for partial functions.
primClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primClasses =
  M.fromList
    [ (C.Partial, makeTypeClassData [] [] [] [] True)
    ]

-- | This contains all of the type classes from all Prim modules.
allPrimClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
allPrimClasses = M.unions
  [ primClasses
  , primCoerceClasses
  , primRowClasses
  , primRowListClasses
  , primSymbolClasses
  , primIntClasses
  , primTypeErrorClasses
  ]

primCoerceClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primCoerceClasses =
  M.fromList
    -- class Coercible (a :: k) (b :: k)
    [ (C.Coercible, makeTypeClassData
        [ ("a", Just (tyVar "k"))
        , ("b", Just (tyVar "k"))
        ] [] [] [] True)
    ]

primRowClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primRowClasses =
  M.fromList
    -- class Union (left :: Row k) (right :: Row k) (union :: Row k) | left right -> union, right union -> left, union left -> right
    [ (C.RowUnion, makeTypeClassData
        [ ("left", Just (kindRow (tyVar "k")))
        , ("right", Just (kindRow (tyVar "k")))
        , ("union", Just (kindRow (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [1, 2] [0]
        , FunctionalDependency [2, 0] [1]
        ] True)

    -- class Nub (original :: Row k) (nubbed :: Row k) | original -> nubbed
    , (C.RowNub, makeTypeClassData
        [ ("original", Just (kindRow (tyVar "k")))
        , ("nubbed", Just (kindRow (tyVar "k")))
        ] [] []
        [ FunctionalDependency [0] [1]
        ] True)

    -- class Lacks (label :: Symbol) (row :: Row k)
    , (C.RowLacks, makeTypeClassData
        [ ("label", Just kindSymbol)
        , ("row", Just (kindRow (tyVar "k")))
        ] [] [] [] True)

    -- class RowCons (label :: Symbol) (a :: k) (tail :: Row k) (row :: Row k) | label tail a -> row, label row -> tail a
    , (C.RowCons, makeTypeClassData
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
    [ (C.RowToList, makeTypeClassData
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
    [ (C.SymbolAppend, makeTypeClassData
        [ ("left", Just kindSymbol)
        , ("right", Just kindSymbol)
        , ("appended", Just kindSymbol)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [1, 2] [0]
        , FunctionalDependency [2, 0] [1]
        ] True)

    -- class Compare (left :: Symbol) (right :: Symbol) (ordering :: Ordering) | left right -> ordering
    , (C.SymbolCompare, makeTypeClassData
        [ ("left", Just kindSymbol)
        , ("right", Just kindSymbol)
        , ("ordering", Just kindOrdering)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        ] True)

    -- class Cons (head :: Symbol) (tail :: Symbol) (symbol :: Symbol) | head tail -> symbol, symbol -> head tail
    , (C.SymbolCons, makeTypeClassData
        [ ("head", Just kindSymbol)
        , ("tail", Just kindSymbol)
        , ("symbol", Just kindSymbol)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [2] [0, 1]
        ] True)
    ]

primIntClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primIntClasses =
  M.fromList
    -- class Add (left :: Int) (right :: Int) (sum :: Int) | left right -> sum, left sum -> right, right sum -> left
    [ (C.IntAdd, makeTypeClassData
        [ ("left", Just tyInt)
        , ("right", Just tyInt)
        , ("sum", Just tyInt)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        , FunctionalDependency [0, 2] [1]
        , FunctionalDependency [1, 2] [0]
        ] True)

    -- class Compare (left :: Int) (right :: Int) (ordering :: Ordering) | left right -> ordering
    , (C.IntCompare, makeTypeClassData
        [ ("left", Just tyInt)
        , ("right", Just tyInt)
        , ("ordering", Just kindOrdering)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        ] True)

    -- class Mul (left :: Int) (right :: Int) (product :: Int) | left right -> product
    , (C.IntMul, makeTypeClassData
        [ ("left", Just tyInt)
        , ("right", Just tyInt)
        , ("product", Just tyInt)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        ] True)

    -- class ToString (int :: Int) (string :: Symbol) | int -> string
    , (C.IntToString, makeTypeClassData
        [ ("int", Just tyInt)
        , ("string", Just kindSymbol)
        ] [] []
        [ FunctionalDependency [0] [1]
        ] True)
    ]

primTypeErrorClasses :: M.Map (Qualified (ProperName 'ClassName)) TypeClassData
primTypeErrorClasses =
  M.fromList
    -- class Fail (message :: Symbol)
    [ (C.Fail, makeTypeClassData
        [("message", Just kindDoc)] [] [] [] True)

    -- class Warn (message :: Symbol)
    , (C.Warn, makeTypeClassData
        [("message", Just kindDoc)] [] [] [] True)
    ]

-- | Finds information about data constructors from the current environment.
lookupConstructor :: Environment -> Qualified (ProperName 'ConstructorName) -> (DataDeclType, ProperName 'TypeName, SourceType, [Ident])
lookupConstructor env ctor =
  fromMaybe (internalError "Data constructor not found") $ ctor `M.lookup` dataConstructors env

-- | Finds information about values from the current environment.
lookupValue :: Environment -> Qualified Ident -> Maybe (SourceType, NameKind, NameVisibility)
lookupValue env ident = ident `M.lookup` names env

dictTypeName' :: Text -> Text
dictTypeName' = (<> "$Dict")

dictTypeName :: ProperName a -> ProperName a
dictTypeName = ProperName . dictTypeName' . runProperName

isDictTypeName :: ProperName a -> Bool
isDictTypeName = T.isSuffixOf "$Dict" . runProperName

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
  go kinds (ForAll _ _ _ _ k _) = go kinds k
  go kinds k = (reverse kinds, k)
