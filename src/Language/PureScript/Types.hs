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
-- The type of types
--
data Type a
  -- | A unification variable of type Type
  = TUnknown Int a
  -- | A named type variable
  | TypeVar Text a
  -- | A type-level string
  | TypeLevelString PSString a
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard SourceSpan a
  -- | A type constructor
  | TypeConstructor (Qualified (ProperName 'TypeName)) a
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp (Qualified (OpName 'TypeOpName)) a
  -- | A type application
  | TypeApp (Type a) (Type a) a
  -- | Forall quantifier
  | ForAll Text (Type a) (Maybe SkolemScope) a
  -- | A type with a set of type class constraints
  | ConstrainedType [Constraint a] (Type a) a
  -- | A skolem constant
  | Skolem Text Int SkolemScope (Maybe SourceSpan) a
  -- | An empty row
  | REmpty a
  -- | A non-empty row
  | RCons Label (Type a) (Type a) a
  -- | A type with a kind annotation
  | KindedType (Type a) Kind a
  -- | A placeholder used in pretty printing
  | PrettyPrintFunction (Type a) (Type a) a
  -- | A placeholder used in pretty printing
  | PrettyPrintObject (Type a) a
  -- | A placeholder used in pretty printing
  | PrettyPrintForAll [Text] (Type a) a
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType (Type a) (Type a) (Type a) a
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType (Type a) a
  deriving (Show, Functor)

instance Eq (Type a) where
  TUnknown            a1          _ == TUnknown            a2          _ = a1 == a2
  TypeVar             a1          _ == TypeVar             a2          _ = a1 == a2
  TypeLevelString     a1          _ == TypeLevelString     a2          _ = a1 == a2
  TypeWildcard        a1          _ == TypeWildcard        a2          _ = a1 == a2
  TypeConstructor     a1          _ == TypeConstructor     a2          _ = a1 == a2
  TypeOp              a1          _ == TypeOp              a2          _ = a1 == a2
  TypeApp             a1 b1       _ == TypeApp             a2 b2       _ = a1 == a2 && b1 == b2
  ForAll              a1 b1 c1    _ == ForAll              a2 b2 c2    _ = a1 == a2 && b1 == b2 && c1 == c2
  ConstrainedType     a1 b1       _ == ConstrainedType     a2 b2       _ = a1 == a2 && b1 == b2
  Skolem              a1 b1 c1 d1 _ == Skolem              a2 b2 c2 d2 _ = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
  REmpty                          _ == REmpty                          _ = True
  RCons               a1 b1 c1    _ == RCons               a2 b2 c2    _ = a1 == a2 && b1 == b2 && c1 == c2
  KindedType          a1 b1       _ == KindedType          a2 b2       _ = a1 == a2 && b1 == b2
  PrettyPrintFunction a1 b1       _ == PrettyPrintFunction a2 b2       _ = a1 == a2 && b1 == b2
  PrettyPrintObject   a1          _ == PrettyPrintObject   a2          _ = a1 == a2
  PrettyPrintForAll   a1 b1       _ == PrettyPrintForAll   a2 b2       _ = a1 == a2 && b1 == b2
  BinaryNoParensType  a1 b1 c1    _ == BinaryNoParensType  a2 b2 c2    _ = a1 == a2 && b1 == b2 && c1 == c2
  ParensInType        a1          _ == ParensInType        a2          _ = a1 == a2
  _ == _ = False

instance Ord (Type a) where
  compare (TUnknown a1 _) (TUnknown a2 _) = compare a1 a2
  compare TUnknown{} _ = LT
  compare _ TUnknown{} = GT

  compare (TypeVar a1 _) (TypeVar a2 _) = compare a1 a2
  compare TypeVar{} _ = LT
  compare _ TypeVar{} = GT

  compare (TypeLevelString a1 _) (TypeLevelString a2 _) = compare a1 a2
  compare TypeLevelString{} _ = LT
  compare _ TypeLevelString{} = GT

  compare (TypeWildcard a1 _) (TypeWildcard a2 _) = compare a1 a2
  compare TypeWildcard{} _ = LT
  compare _ TypeWildcard{} = GT

  compare (TypeConstructor a1 _) (TypeConstructor a2 _) = compare a1 a2
  compare TypeConstructor{} _ = LT
  compare _ TypeConstructor{} = GT

  compare (TypeOp a1 _) (TypeOp a2 _) = compare a1 a2
  compare TypeOp{} _ = LT
  compare _ TypeOp{} = GT

  compare (TypeApp a1 b1 _) (TypeApp a2 b2 _) = compare a1 a2 <> compare b1 b2
  compare TypeApp{} _ = LT
  compare _ TypeApp{} = GT

  compare (ForAll a1 b1 c1 _) (ForAll a2 b2 c2 _) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare ForAll{} _ = LT
  compare _ ForAll{} = GT

  compare (ConstrainedType a1 b1 _ ) (ConstrainedType a2 b2 _) = compare a1 a2 <> compare b1 b2
  compare ConstrainedType{} _ = LT
  compare _ ConstrainedType{} = GT

  compare (Skolem a1 b1 c1 d1 _) (Skolem a2 b2 c2 d2 _ ) = compare a1 a2 <> compare b1 b2 <> compare c1 c2 <> compare d1 d2
  compare Skolem{} _ = LT
  compare _ Skolem{} = GT

  compare (REmpty _) (REmpty _) = EQ
  compare REmpty{} _ = LT
  compare _ REmpty{} = GT

  compare (RCons a1 b1 c1 _) (RCons a2 b2 c2 _) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare RCons{} _ = LT
  compare _ RCons{} = GT

  compare (KindedType a1 b1 _) (KindedType a2 b2 _) = compare a1 a2 <> compare b1 b2
  compare KindedType{} _ = LT
  compare _ KindedType{} = GT

  compare (PrettyPrintFunction a1 b1 _) (PrettyPrintFunction a2 b2 _) = compare a1 a2 <> compare b1 b2
  compare PrettyPrintFunction{} _ = LT
  compare _ PrettyPrintFunction{} = GT

  compare (PrettyPrintObject   a1 _) (PrettyPrintObject a2 _) = compare a1 a2
  compare PrettyPrintObject{} _ = LT
  compare _ PrettyPrintObject{} = GT

  compare (PrettyPrintForAll   a1 b1 _) (PrettyPrintForAll a2 b2 _) = compare a1 a2 <> compare b1 b2
  compare PrettyPrintForAll{} _ = LT
  compare _ PrettyPrintForAll{} = GT

  compare (BinaryNoParensType  a1 b1 c1 _) (BinaryNoParensType a2 b2 c2 _) = compare a1 a2 <> compare b1 b2 <> compare c1 c2
  compare BinaryNoParensType{} _ = LT
  compare _ BinaryNoParensType{} = GT

  compare (ParensInType a1 _) (ParensInType a2 _) = compare a1 a2

-- | Extracts the annotation field from a `Type`
extractTypeAnn :: Type a -> a
extractTypeAnn (TUnknown _ a) = a
extractTypeAnn (TypeVar _ a) = a
extractTypeAnn (TypeLevelString _ a) = a
extractTypeAnn (TypeWildcard _ a) = a
extractTypeAnn (TypeConstructor _ a) = a
extractTypeAnn (TypeOp _ a) = a
extractTypeAnn (TypeApp _ _ a) = a
extractTypeAnn (ForAll _ _ _ a) = a
extractTypeAnn (ConstrainedType _ _ a) = a
extractTypeAnn (Skolem _ _ _ _ a) = a
extractTypeAnn (REmpty a) = a
extractTypeAnn (RCons _ _ _ a) = a
extractTypeAnn (KindedType _ _ a) = a
extractTypeAnn (PrettyPrintFunction _ _ a) = a
extractTypeAnn (PrettyPrintObject _ a) = a
extractTypeAnn (PrettyPrintForAll _ _ a) = a
extractTypeAnn (BinaryNoParensType _ _ _ a) = a
extractTypeAnn (ParensInType _ a) = a

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
rowToList (RCons name ty row _) =
  let (tys, rest) = rowToList row
  in ((name, ty):tys, rest)
rowToList r = ([], r)

-- |
-- Convert a list of labels and types to a row
--
rowFromList :: ([(Label, Type a)], Type a) -> Type a
rowFromList ([], r) = r
rowFromList ((name, t):ts, r) = RCons name t (rowFromList (ts, r)) (extractTypeAnn r)

-- |
-- Check whether a type is a monotype
--
isMonoType :: Type a -> Bool
isMonoType ForAll{} = False
isMonoType (ParensInType t _) = isMonoType t
isMonoType (KindedType t _ _) = isMonoType t
isMonoType _ = True

-- |
-- Universally quantify a type
--
mkForAll :: [Text] -> a -> Type a -> Type a
mkForAll args a ty = foldl (\t arg -> ForAll arg t Nothing a) ty args

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
  go _  m (TypeVar v a) = fromMaybe (TypeVar v a) (v `lookup` m)
  go bs m (TypeApp t1 t2 a) = TypeApp (go bs m t1) (go bs m t2) a
  go bs m f@(ForAll v t sco a)
    | v `elem` keys = go bs (filter ((/= v) . fst) m) f
    | v `elem` usedVars =
        let v' = genName v (keys ++ bs ++ usedVars)
            t' = go bs [(v, TypeVar v' a)] t
        in ForAll v' (go (v' : bs) m t') sco a
    | otherwise = ForAll v (go (v : bs) m t) sco a
    where
    keys = map fst m
    usedVars = concatMap (usedTypeVariables . snd) m
  go bs m (ConstrainedType cs t a) = ConstrainedType (map (mapConstraintArgs (map (go bs m))) cs) (go bs m t) a
  go bs m (RCons name' t r a) = RCons name' (go bs m t) (go bs m r) a
  go bs m (KindedType t k a) = KindedType (go bs m t) k a
  go bs m (BinaryNoParensType t1 t2 t3 a) = BinaryNoParensType (go bs m t1) (go bs m t2) (go bs m t3) a
  go bs m (ParensInType t a) = ParensInType (go bs m t) a
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
  go (TypeVar v _) = [v]
  go _ = []

-- |
-- Collect all free type variables appearing in a type
--
freeTypeVariables :: Type a -> [Text]
freeTypeVariables = nub . go []
  where
  go :: [Text] -> Type a -> [Text]
  go bound (TypeVar v _) | v `notElem` bound = [v]
  go bound (TypeApp t1 t2 _) = go bound t1 ++ go bound t2
  go bound (ForAll v t _ _) = go (v : bound) t
  go bound (ConstrainedType cs t _) = concatMap (concatMap (go bound) . constraintArgs) cs ++ go bound t
  go bound (RCons _ t r _) = go bound t ++ go bound r
  go bound (KindedType t _ _) = go bound t
  go bound (BinaryNoParensType t1 t2 t3 _) = go bound t1 ++ go bound t2 ++ go bound t3
  go bound (ParensInType t _) = go bound t
  go _ _ = []

-- |
-- Universally quantify over all type variables appearing free in a type
--
quantify :: a -> Type a -> Type a
quantify a ty = foldr (\arg t -> ForAll arg t Nothing a) ty $ freeTypeVariables ty

-- |
-- Move all universal quantifiers to the front of a type
--
moveQuantifiersToFront :: Type a -> Type a
moveQuantifiersToFront = go [] []
  where
  go qs cs (ForAll q ty sco _) = go ((q, sco) : qs) cs ty
  go qs cs (ConstrainedType cs' ty _) = go qs (cs ++ cs') ty
  go qs cs ty =
    let constrained = case cs of
                        [] -> ty
                        cs' -> ConstrainedType cs' ty (extractTypeAnn ty)
    in case qs of
         [] -> constrained
         qs' -> foldl (\ty' (q, sco) -> ForAll q ty' sco (extractTypeAnn ty')) constrained qs'

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
  go (TypeApp t1 t2 a) = f (TypeApp (go t1) (go t2) a)
  go (ForAll arg ty sco a) = f (ForAll arg (go ty) sco a)
  go (ConstrainedType cs ty a) = f (ConstrainedType (map (mapConstraintArgs (map go)) cs) (go ty) a)
  go (RCons name ty rest a) = f (RCons name (go ty) (go rest) a)
  go (KindedType ty k a) = f (KindedType (go ty) k a)
  go (PrettyPrintFunction t1 t2 a) = f (PrettyPrintFunction (go t1) (go t2) a)
  go (PrettyPrintObject t a) = f (PrettyPrintObject (go t) a)
  go (PrettyPrintForAll args t a) = f (PrettyPrintForAll args (go t) a)
  go (BinaryNoParensType t1 t2 t3 a) = f (BinaryNoParensType (go t1) (go t2) (go t3) a)
  go (ParensInType t a) = f (ParensInType (go t) a)
  go other = f other

everywhereOnTypesTopDown :: (Type a -> Type a) -> Type a -> Type a
everywhereOnTypesTopDown f = go . f
  where
  go (TypeApp t1 t2 a) = TypeApp (go (f t1)) (go (f t2)) a
  go (ForAll arg ty sco a) = ForAll arg (go (f ty)) sco a
  go (ConstrainedType cs ty a) = ConstrainedType (map (mapConstraintArgs (map (go . f))) cs) (go (f ty)) a
  go (RCons name ty rest a) = RCons name (go (f ty)) (go (f rest)) a
  go (KindedType ty k a) = KindedType (go (f ty)) k a
  go (PrettyPrintFunction t1 t2 a) = PrettyPrintFunction (go (f t1)) (go (f t2)) a
  go (PrettyPrintObject t a) = PrettyPrintObject (go (f t)) a
  go (PrettyPrintForAll args t a) = PrettyPrintForAll args (go (f t)) a
  go (BinaryNoParensType t1 t2 t3 a) = BinaryNoParensType (go (f t1)) (go (f t2)) (go (f t3)) a
  go (ParensInType t a) = ParensInType (go (f t)) a
  go other = f other

everywhereOnTypesM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesM f = go
  where
  go (TypeApp t1 t2 a) = (TypeApp <$> go t1 <*> go t2 <*> pure a) >>= f
  go (ForAll arg ty sco a) = (ForAll arg <$> go ty <*> pure sco <*> pure a) >>= f
  go (ConstrainedType cs ty a) = (ConstrainedType <$> mapM (overConstraintArgs (mapM go)) cs <*> go ty <*> pure a) >>= f
  go (RCons name ty rest a) = (RCons name <$> go ty <*> go rest <*> pure a) >>= f
  go (KindedType ty k a) = (KindedType <$> go ty <*> pure k <*> pure a) >>= f
  go (PrettyPrintFunction t1 t2 a) = (PrettyPrintFunction <$> go t1 <*> go t2 <*> pure a) >>= f
  go (PrettyPrintObject t a) = (PrettyPrintObject <$> go t <*> pure a) >>= f
  go (PrettyPrintForAll args t a) = (PrettyPrintForAll args <$> go t <*> pure a) >>= f
  go (BinaryNoParensType t1 t2 t3 a) = (BinaryNoParensType <$> go t1 <*> go t2 <*> go t3 <*> pure a) >>= f
  go (ParensInType t a) = (ParensInType <$> go t <*> pure a) >>= f
  go other = f other

everywhereOnTypesTopDownM :: Monad m => (Type a -> m (Type a)) -> Type a -> m (Type a)
everywhereOnTypesTopDownM f = go <=< f
  where
  go (TypeApp t1 t2 a) = TypeApp <$> (f t1 >>= go) <*> (f t2 >>= go) <*> pure a
  go (ForAll arg ty sco a) = ForAll arg <$> (f ty >>= go) <*> pure sco <*> pure a
  go (ConstrainedType cs ty a) = ConstrainedType <$> mapM (overConstraintArgs (mapM (go <=< f))) cs <*> (f ty >>= go) <*> pure a
  go (RCons name ty rest a) = RCons name <$> (f ty >>= go) <*> (f rest >>= go) <*> pure a
  go (KindedType ty k a) = KindedType <$> (f ty >>= go) <*> pure k <*> pure a
  go (PrettyPrintFunction t1 t2 a) = PrettyPrintFunction <$> (f t1 >>= go) <*> (f t2 >>= go) <*> pure a
  go (PrettyPrintObject t a) = PrettyPrintObject <$> (f t >>= go) <*> pure a
  go (PrettyPrintForAll args t a) = PrettyPrintForAll args <$> (f t >>= go) <*> pure a
  go (BinaryNoParensType t1 t2 t3 a) = BinaryNoParensType <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go) <*> pure a
  go (ParensInType t a) = ParensInType <$> (f t >>= go) <*> pure a
  go other = f other

everythingOnTypes :: (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes (<+>) f = go
  where
  go t@(TypeApp t1 t2 _) = f t <+> go t1 <+> go t2
  go t@(ForAll _ ty _ _) = f t <+> go ty
  go t@(ConstrainedType cs ty _) = foldl (<+>) (f t) (map go $ concatMap constraintArgs cs) <+> go ty
  go t@(RCons _ ty rest _) = f t <+> go ty <+> go rest
  go t@(KindedType ty _ _) = f t <+> go ty
  go t@(PrettyPrintFunction t1 t2 _) = f t <+> go t1 <+> go t2
  go t@(PrettyPrintObject t1 _) = f t <+> go t1
  go t@(PrettyPrintForAll _ t1 _) = f t <+> go t1
  go t@(BinaryNoParensType t1 t2 t3 _) = f t <+> go t1 <+> go t2 <+> go t3
  go t@(ParensInType t1 _) = f t <+> go t1
  go other = f other

everythingWithContextOnTypes :: s -> r -> (r -> r -> r) -> (s -> Type a -> (s, r)) -> Type a -> r
everythingWithContextOnTypes s0 r0 (<+>) f = go' s0
  where
  go' s t = let (s', r) = f s t in r <+> go s' t
  go s (TypeApp t1 t2 _) = go' s t1 <+> go' s t2
  go s (ForAll _ ty _ _) = go' s ty
  go s (ConstrainedType cs ty _) = foldl (<+>) r0 (map (go' s) $ concatMap constraintArgs cs) <+> go' s ty
  go s (RCons _ ty rest _) = go' s ty <+> go' s rest
  go s (KindedType ty _ _) = go' s ty
  go s (PrettyPrintFunction t1 t2 _) = go' s t1 <+> go' s t2
  go s (PrettyPrintObject t1 _) = go' s t1
  go s (PrettyPrintForAll _ t1 _) = go' s t1
  go s (BinaryNoParensType t1 t2 t3 _) = go' s t1 <+> go' s t2 <+> go' s t3
  go s (ParensInType t1 _) = go' s t1
  go _ _ = r0
