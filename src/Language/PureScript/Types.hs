{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for types
--
module Language.PureScript.Types where

import Prelude.Compat

import Control.Monad ((<=<))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Label (Label)
import Language.PureScript.PSString (PSString)

-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope { runSkolemScope :: Int }
  deriving (Show, Eq, Ord, A.ToJSON, A.FromJSON)

-- |
-- The type of types. The @a@ parameters allows for types to be annotated with
-- extra data - source position info, for example.
--
data Type a
  -- | A unification variable of type Type
  = TUnknown a Int
  -- | A named type variable
  | TypeVar a Text
  -- | A type-level string
  | TypeLevelString a PSString
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard a SourceSpan
  -- | A type constructor
  | TypeConstructor a (Qualified (ProperName 'TypeName))
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp a (Qualified (OpName 'TypeOpName))
  -- | A type application
  | TypeApp a (Type a) (Type a)
  -- | Forall quantifier
  | ForAll a Text (Type a) (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType a [Constraint a] (Type a)
  -- | A skolem constant
  | Skolem a Text Int SkolemScope (Maybe SourceSpan)
  -- | An empty row
  | REmpty a
  -- | A non-empty row
  | RCons a Label (Type a) (Type a)
  -- | A type with a kind annotation
  | KindedType a (Type a) Kind
  -- | A placeholder used in pretty printing
  | PrettyPrintFunction a (Type a) (Type a)
  -- | A placeholder used in pretty printing
  | PrettyPrintObject a (Type a)
  -- | A placeholder used in pretty printing
  | PrettyPrintForAll a [Text] (Type a)
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType a (Type a) (Type a) (Type a)
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType a (Type a)
  deriving (Show, Functor)

instance Eq (Type a) where
  TUnknown            _ a1          == TUnknown            _ a2          = a1 == a2
  TypeVar             _ a1          == TypeVar             _ a2          = a1 == a2
  TypeLevelString     _ a1          == TypeLevelString     _ a2          = a1 == a2
  TypeWildcard        _ a1          == TypeWildcard        _ a2          = a1 == a2
  TypeConstructor     _ a1          == TypeConstructor     _ a2          = a1 == a2
  TypeOp              _ a1          == TypeOp              _ a2          = a1 == a2
  TypeApp             _ a1 b1       == TypeApp             _ a2 b2       = a1 == a2 && b1 == b2
  ForAll              _ a1 b1 c1    == ForAll              _ a2 b2 c2    = a1 == a2 && b1 == b2 && c1 == c2
  ConstrainedType     _ a1 b1       == ConstrainedType     _ a2 b2       = a1 == a2 && b1 == b2
  Skolem              _ a1 b1 c1 d1 == Skolem              _ a2 b2 c2 d2 = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
  REmpty              _             == REmpty              _             = True
  RCons               _ a1 b1 c1    == RCons               _ a2 b2 c2    = a1 == a2 && b1 == b2 && c1 == c2
  KindedType          _ a1 b1       == KindedType          _ a2 b2       = a1 == a2 && b1 == b2
  PrettyPrintFunction _ a1 b1       == PrettyPrintFunction _ a2 b2       = a1 == a2 && b1 == b2
  PrettyPrintObject   _ a1          == PrettyPrintObject   _ a2          = a1 == a2
  PrettyPrintForAll   _ a1 b1       == PrettyPrintForAll   _ a2 b2       = a1 == a2 && b1 == b2
  BinaryNoParensType  _ a1 b1 c1    == BinaryNoParensType  _ a2 b2 c2    = a1 == a2 && b1 == b2 && c1 == c2
  ParensInType        _ a1          == ParensInType        _ a2          = a1 == a2
  _ == _ = False

instance Ord (Type a) where
  compare (TUnknown _ a1) (TUnknown _ a2) = compare a1 a2
  compare TUnknown{} _ = LT
  compare _ TUnknown{} = GT

  compare (TypeVar _ a1) (TypeVar _ a2) = compare a1 a2
  compare TypeVar{} _ = LT
  compare _ TypeVar{} = GT

  compare (TypeLevelString _ a1) (TypeLevelString _ a2) = compare a1 a2
  compare TypeLevelString{} _ = LT
  compare _ TypeLevelString{} = GT

  compare (TypeWildcard _ a1) (TypeWildcard _ a2) = compare a1 a2
  compare TypeWildcard{} _ = LT
  compare _ TypeWildcard{} = GT

  compare (TypeConstructor _ a1) (TypeConstructor _ a2) = compare a1 a2
  compare TypeConstructor{} _ = LT
  compare _ TypeConstructor{} = GT

  compare (TypeOp _ a1) (TypeOp _ a2) = compare a1 a2
  compare TypeOp{} _ = LT
  compare _ TypeOp{} = GT

  compare (TypeApp _ a1 b1) (TypeApp _ a2 b2) = compare a1 a2 <> compare b1 b2
  compare TypeApp{} _ = LT
  compare _ TypeApp{} = GT

  compare (ForAll _ a1 b1 c1) (ForAll _ a2 b2 c2) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare ForAll{} _ = LT
  compare _ ForAll{} = GT

  compare (ConstrainedType _ a1 b1) (ConstrainedType _ a2 b2) = compare a1 a2 <> compare b1 b2
  compare ConstrainedType{} _ = LT
  compare _ ConstrainedType{} = GT

  compare (Skolem _ a1 b1 c1 d1) (Skolem _ a2 b2 c2 d2) = compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2
  compare Skolem{} _ = LT
  compare _ Skolem{} = GT

  compare (REmpty _) (REmpty _) = EQ
  compare REmpty{} _ = LT
  compare _ REmpty{} = GT

  compare (RCons _ a1 b1 c1) (RCons _ a2 b2 c2) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare RCons{} _ = LT
  compare _ RCons{} = GT

  compare (KindedType _ a1 b1) (KindedType _ a2 b2) = compare a1 a2 <> compare b1 b2
  compare KindedType{} _ = LT
  compare _ KindedType{} = GT

  compare (PrettyPrintFunction _ a1 b1) (PrettyPrintFunction _ a2 b2) = compare a1 a2 <> compare b1 b2
  compare PrettyPrintFunction{} _ = LT
  compare _ PrettyPrintFunction{} = GT

  compare (PrettyPrintObject _ a1) (PrettyPrintObject _ a2) = compare a1 a2
  compare PrettyPrintObject{} _ = LT
  compare _ PrettyPrintObject{} = GT

  compare (PrettyPrintForAll _ a1 b1) (PrettyPrintForAll _ a2 b2) = compare a1 a2 <> compare b1 b2
  compare PrettyPrintForAll{} _ = LT
  compare _ PrettyPrintForAll{} = GT

  compare (BinaryNoParensType _ a1 b1 c1) (BinaryNoParensType _ a2 b2 c2) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare BinaryNoParensType{} _ = LT
  compare _ BinaryNoParensType{} = GT

  compare (ParensInType _ a1) (ParensInType _ a2) = compare a1 a2

-- | Extracts the annotation field from a `Type`
extractTypeAnn :: Type a -> a
extractTypeAnn (TUnknown ann _) = ann
extractTypeAnn (TypeVar ann _) = ann
extractTypeAnn (TypeLevelString ann _) = ann
extractTypeAnn (TypeWildcard ann _) = ann
extractTypeAnn (TypeConstructor ann _) = ann
extractTypeAnn (TypeOp ann _) = ann
extractTypeAnn (TypeApp ann _ _) = ann
extractTypeAnn (ForAll ann _ _ _) = ann
extractTypeAnn (ConstrainedType ann _ _) = ann
extractTypeAnn (Skolem ann _ _ _ _) = ann
extractTypeAnn (REmpty ann) = ann
extractTypeAnn (RCons ann _ _ _) = ann
extractTypeAnn (KindedType ann _ _) = ann
extractTypeAnn (PrettyPrintFunction ann _ _) = ann
extractTypeAnn (PrettyPrintObject ann _) = ann
extractTypeAnn (PrettyPrintForAll ann _ _) = ann
extractTypeAnn (BinaryNoParensType ann _ _ _) = ann
extractTypeAnn (ParensInType ann _) = ann

-- | Additional data relevant to type class constraints
data ConstraintData
  = PartialConstraintData [[Text]] Bool
  -- ^ Data to accompany a Partial constraint generated by the exhaustivity checker.
  -- It contains (rendered) binder information for those binders which were
  -- not matched, and a flag indicating whether the list was truncated or not.
  -- Note: we use 'String' here because using 'Binder' would introduce a cyclic
  -- dependency in the module graph.
  deriving (Show, Eq, Ord)

-- | A typeclass constraint
data Constraint a = Constraint
  { constraintClass :: Qualified (ProperName 'ClassName)
  -- ^ constraint class name
  , constraintArgs  :: [Type a]
  -- ^ type arguments
  , constraintData  :: Maybe ConstraintData
  -- ^ additional data relevant to this constraint
  } deriving (Show, Eq, Ord, Functor)

mapConstraintArgs :: ([Type a] -> [Type a]) -> Constraint a -> Constraint a
mapConstraintArgs f c = c { constraintArgs = f (constraintArgs c) }

overConstraintArgs :: Functor f => ([Type a] -> f [Type a]) -> Constraint a -> f (Constraint a)
overConstraintArgs f c = (\args -> c { constraintArgs = args }) <$> f (constraintArgs c)

$(A.deriveJSON A.defaultOptions ''Type)
$(A.deriveJSON A.defaultOptions ''Constraint)
$(A.deriveJSON A.defaultOptions ''ConstraintData)

-- |
-- Convert a row to a list of pairs of labels and types
--
rowToList :: Type a -> ([(Label, Type a)], Type a)
rowToList (RCons _ name ty row) =
  let (tys, rest) = rowToList row
  in ((name, ty):tys, rest)
rowToList r = ([], r)

-- |
-- Convert a list of labels and types to a row
--
rowFromList :: ([(Label, Type a)], Type a) -> Type a
rowFromList ([], r) = r
rowFromList ((name, t):ts, r) = RCons (extractTypeAnn r) name t (rowFromList (ts, r))

-- |
-- Check whether a type is a monotype
--
isMonoType :: Type a -> Bool
isMonoType ForAll{} = False
isMonoType (ParensInType _ t) = isMonoType t
isMonoType (KindedType _ t _) = isMonoType t
isMonoType _ = True

-- |
-- Universally quantify a type
--
mkForAll :: [Text] -> a -> Type a -> Type a
mkForAll args ann ty = foldl (\t arg -> ForAll ann arg t Nothing) ty args

-- |
-- Replace a type variable, taking into account variable shadowing
--
replaceTypeVars :: Text -> Type a -> Type a -> Type a
replaceTypeVars v r = replaceAllTypeVars [(v, r)]

-- |
-- Replace named type variables with types
--
replaceAllTypeVars :: [(Text, Type a)] -> Type a -> Type a
replaceAllTypeVars = go []
  where

  go :: [Text] -> [(Text, Type a)] -> Type a -> Type a
  go _  m (TypeVar ann v) = fromMaybe (TypeVar ann v) (v `lookup` m)
  go bs m (TypeApp ann t1 t2) = TypeApp ann (go bs m t1) (go bs m t2)
  go bs m f@(ForAll ann v t sco)
    | v `elem` keys = go bs (filter ((/= v) . fst) m) f
    | v `elem` usedVars =
        let v' = genName v (keys ++ bs ++ usedVars)
            t' = go bs [(v, TypeVar ann v')] t
        in ForAll ann v' (go (v' : bs) m t') sco
    | otherwise = ForAll ann v (go (v : bs) m t) sco
    where
    keys = map fst m
    usedVars = concatMap (usedTypeVariables . snd) m
  go bs m (ConstrainedType ann cs t) = ConstrainedType ann (map (mapConstraintArgs (map (go bs m))) cs) (go bs m t)
  go bs m (RCons ann name' t r) = RCons ann name' (go bs m t) (go bs m r)
  go bs m (KindedType ann t k) = KindedType ann (go bs m t) k
  go bs m (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go bs m t1) (go bs m t2) (go bs m t3)
  go bs m (ParensInType ann t) = ParensInType ann (go bs m t)
  go _  _ ty = ty

  genName orig inUse = try' 0
    where
    try' :: Integer -> Text
    try' n | (orig <> T.pack (show n)) `elem` inUse = try' (n + 1)
           | otherwise = orig <> T.pack (show n)

-- |
-- Collect all type variables appearing in a type
--
usedTypeVariables :: Type a -> [Text]
usedTypeVariables = nub . everythingOnTypes (++) go
  where
  go (TypeVar _ v) = [v]
  go _ = []

-- |
-- Collect all free type variables appearing in a type
--
freeTypeVariables :: Type a -> [Text]
freeTypeVariables = nub . go []
  where
  go :: [Text] -> Type a -> [Text]
  go bound (TypeVar _ v) | v `notElem` bound = [v]
  go bound (TypeApp _ t1 t2) = go bound t1 ++ go bound t2
  go bound (ForAll _ v t _) = go (v : bound) t
  go bound (ConstrainedType _ cs t) = concatMap (concatMap (go bound) . constraintArgs) cs ++ go bound t
  go bound (RCons _ _ t r) = go bound t ++ go bound r
  go bound (KindedType _ t _) = go bound t
  go bound (BinaryNoParensType _ t1 t2 t3) = go bound t1 ++ go bound t2 ++ go bound t3
  go bound (ParensInType _ t) = go bound t
  go _ _ = []

-- |
-- Universally quantify over all type variables appearing free in a type
--
quantify :: a -> Type a -> Type a
quantify ann ty = foldr (\arg t -> ForAll ann arg t Nothing) ty $ freeTypeVariables ty

-- |
-- Move all universal quantifiers to the front of a type
--
moveQuantifiersToFront :: Type a -> Type a
moveQuantifiersToFront = go [] []
  where
  go qs cs (ForAll _ q ty sco) = go ((q, sco) : qs) cs ty
  go qs cs (ConstrainedType _ cs' ty) = go qs (cs ++ cs') ty
  go qs cs ty =
    let constrained = case cs of
                        [] -> ty
                        cs' -> ConstrainedType (extractTypeAnn ty) cs' ty
    in case qs of
         [] -> constrained
         qs' -> foldl (\ty' (q, sco) -> ForAll (extractTypeAnn ty') q ty' sco) constrained qs'

-- |
-- Check if a type contains wildcards
--
containsWildcards :: Type a -> Bool
containsWildcards = everythingOnTypes (||) go
  where
  go :: Type a -> Bool
  go TypeWildcard{} = True
  go _ = False

--
-- Traversals
--

everywhereOnTypes :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypes f = go
  where
  go (TypeApp ann t1 t2) = f (TypeApp ann (go t1) (go t2))
  go (ForAll ann arg ty sco) = f (ForAll ann arg (go ty) sco)
  go (ConstrainedType ann cs ty) = f (ConstrainedType ann (map (mapConstraintArgs (map go)) cs) (go ty))
  go (RCons ann name ty rest) = f (RCons ann name (go ty) (go rest))
  go (KindedType ann ty k) = f (KindedType ann (go ty) k)
  go (PrettyPrintFunction ann t1 t2) = f (PrettyPrintFunction ann (go t1) (go t2))
  go (PrettyPrintObject ann t) = f (PrettyPrintObject ann (go t))
  go (PrettyPrintForAll ann args t) = f (PrettyPrintForAll ann args (go t))
  go (BinaryNoParensType ann t1 t2 t3) = f (BinaryNoParensType ann (go t1) (go t2) (go t3))
  go (ParensInType ann t) = f (ParensInType ann (go t))
  go other = f other

everywhereOnTypesTopDown :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypesTopDown f = go . f
  where
  go (TypeApp ann t1 t2) = TypeApp ann (go (f t1)) (go (f t2))
  go (ForAll ann arg ty sco) = ForAll ann arg (go (f ty)) sco
  go (ConstrainedType ann cs ty) = ConstrainedType ann (map (mapConstraintArgs (map (go . f))) cs) (go (f ty))
  go (RCons ann name ty rest) = RCons ann name (go (f ty)) (go (f rest))
  go (KindedType ann ty k) = KindedType ann (go (f ty)) k
  go (PrettyPrintFunction ann t1 t2) = PrettyPrintFunction ann (go (f t1)) (go (f t2))
  go (PrettyPrintObject ann t) = PrettyPrintObject ann (go (f t))
  go (PrettyPrintForAll ann args t) = PrettyPrintForAll ann args (go (f t))
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann (go (f t1)) (go (f t2)) (go (f t3))
  go (ParensInType ann t) = ParensInType ann (go (f t))
  go other = f other

everywhereOnTypesM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesM f = go
  where
  go (TypeApp ann t1 t2) = (TypeApp ann <$> go t1 <*> go t2) >>= f
  go (ForAll ann arg ty sco) = (ForAll ann arg <$> go ty <*> pure sco) >>= f
  go (ConstrainedType ann cs ty) = (ConstrainedType ann <$> mapM (overConstraintArgs (mapM go)) cs <*> go ty) >>= f
  go (RCons ann name ty rest) = (RCons ann name <$> go ty <*> go rest) >>= f
  go (KindedType ann ty k) = (KindedType ann <$> go ty <*> pure k) >>= f
  go (PrettyPrintFunction ann t1 t2) = (PrettyPrintFunction ann <$> go t1 <*> go t2) >>= f
  go (PrettyPrintObject ann t) = (PrettyPrintObject ann <$> go t) >>= f
  go (PrettyPrintForAll ann args t) = (PrettyPrintForAll ann args <$> go t) >>= f
  go (BinaryNoParensType ann t1 t2 t3) = (BinaryNoParensType ann <$> go t1 <*> go t2 <*> go t3) >>= f
  go (ParensInType ann t) = (ParensInType ann <$> go t) >>= f
  go other = f other

everywhereOnTypesTopDownM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesTopDownM f = go <=< f
  where
  go (TypeApp ann t1 t2) = TypeApp ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (ForAll ann arg ty sco) = ForAll ann arg <$> (f ty >>= go) <*> pure sco
  go (ConstrainedType ann cs ty) = ConstrainedType ann <$> mapM (overConstraintArgs (mapM (go <=< f))) cs <*> (f ty >>= go)
  go (RCons ann name ty rest) = RCons ann name <$> (f ty >>= go) <*> (f rest >>= go)
  go (KindedType ann ty k) = KindedType ann <$> (f ty >>= go) <*> pure k
  go (PrettyPrintFunction ann t1 t2) = PrettyPrintFunction ann <$> (f t1 >>= go) <*> (f t2 >>= go)
  go (PrettyPrintObject ann t) = PrettyPrintObject ann <$> (f t >>= go)
  go (PrettyPrintForAll ann args t) = PrettyPrintForAll ann args <$> (f t >>= go)
  go (BinaryNoParensType ann t1 t2 t3) = BinaryNoParensType ann <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
  go (ParensInType ann t) = ParensInType ann <$> (f t >>= go)
  go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes (<+>) f = go
  where
  go t@(TypeApp _ t1 t2) = f t <+> go t1 <+> go t2
  go t@(ForAll _ _ ty _) = f t <+> go ty
  go t@(ConstrainedType _ cs ty) = foldl (<+>) (f t) (map go $ concatMap constraintArgs cs) <+> go ty
  go t@(RCons _ _ ty rest) = f t <+> go ty <+> go rest
  go t@(KindedType _ ty _) = f t <+> go ty
  go t@(PrettyPrintFunction _ t1 t2) = f t <+> go t1 <+> go t2
  go t@(PrettyPrintObject _ t1) = f t <+> go t1
  go t@(PrettyPrintForAll _ _ t1) = f t <+> go t1
  go t@(BinaryNoParensType _ t1 t2 t3) = f t <+> go t1 <+> go t2 <+> go t3
  go t@(ParensInType _ t1) = f t <+> go t1
  go other = f other

everythingWithContextOnTypes :: s -> r -> (r -> r -> r) -> (s -> Type a -> (s, r)) -> Type a -> r
everythingWithContextOnTypes s0 r0 (<+>) f = go' s0
  where
  go' s t = let (s', r) = f s t in r <+> go s' t
  go s (TypeApp _ t1 t2) = go' s t1 <+> go' s t2
  go s (ForAll _ _ ty _) = go' s ty
  go s (ConstrainedType _ cs ty) = foldl (<+>) r0 (map (go' s) $ concatMap constraintArgs cs) <+> go' s ty
  go s (RCons _ _ ty rest) = go' s ty <+> go' s rest
  go s (KindedType _ ty _) = go' s ty
  go s (PrettyPrintFunction _ t1 t2) = go' s t1 <+> go' s t2
  go s (PrettyPrintObject _ t1) = go' s t1
  go s (PrettyPrintForAll _ _ t1) = go' s t1
  go s (BinaryNoParensType _ t1 t2 t3) = go' s t1 <+> go' s t2 <+> go' s t3
  go s (ParensInType _ t1) = go' s t1
  go _ _ = r0
