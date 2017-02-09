-- |
-- AST traversal helpers
--
module Language.PureScript.AST.Traversals where

import Prelude.Compat

import Control.Monad

import Data.Foldable (fold)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Literals
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.Types

guardedExprM :: Applicative m
             => (Guard -> m Guard)
             -> (Expr -> m Expr)
             -> GuardedExpr
             -> m GuardedExpr
guardedExprM f g (GuardedExpr guards rhs) =
  GuardedExpr <$> traverse f guards <*> g rhs

mapGuardedExpr :: (Guard -> Guard)
               -> (Expr -> Expr)
               -> GuardedExpr
               -> GuardedExpr
mapGuardedExpr f g (GuardedExpr guards rhs) =
  GuardedExpr (map f guards) (g rhs)

everywhereOnValues
  :: (Declaration -> Declaration)
  -> (Expr -> Expr)
  -> (Binder -> Binder)
  -> ( Declaration -> Declaration
     , Expr -> Expr
     , Binder -> Binder
     )
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration name nameKind bs val) = f (ValueDeclaration name nameKind (map h' bs) (map (mapGuardedExpr handleGuard g') val))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration name args implies deps ds) = f (TypeClassDeclaration name args implies deps (map f' ds))
  f' (TypeInstanceDeclaration name cs className args ds) = f (TypeInstanceDeclaration name cs className args (mapTypeInstanceBody (map f') ds))
  f' (PositionedDeclaration pos com d) = f (PositionedDeclaration pos com (f' d))
  f' other = f other

  g' :: Expr -> Expr
  g' (Literal l) = g (Literal (lit g' l))
  g' (UnaryMinus v) = g (UnaryMinus (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens (g' op) (g' v1) (g' v2))
  g' (Parens v) = g (Parens (g' v))
  g' (TypeClassDictionaryConstructorApp name v) = g (TypeClassDictionaryConstructorApp name (g' v))
  g' (Accessor prop v) = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (fmap g') vs))
  g' (ObjectUpdateNested obj vs) = g (ObjectUpdateNested (g' obj) (fmap g' vs))
  g' (Abs name v) = g (Abs name (g' v))
  g' (App v1 v2) = g (App (g' v1) (g' v2))
  g' (IfThenElse v1 v2 v3) = g (IfThenElse (g' v1) (g' v2) (g' v3))
  g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
  g' (TypedValue check v ty) = g (TypedValue check (g' v) ty)
  g' (Let ds v) = g (Let (map f' ds) (g' v))
  g' (Do es) = g (Do (map handleDoNotationElement es))
  g' (PositionedValue pos com v) = g (PositionedValue pos com (g' v))
  g' other = g other

  h' :: Binder -> Binder
  h' (ConstructorBinder ctor bs) = h (ConstructorBinder ctor (map h' bs))
  h' (BinaryNoParensBinder b1 b2 b3) = h (BinaryNoParensBinder (h' b1) (h' b2) (h' b3))
  h' (ParensInBinder b) = h (ParensInBinder (h' b))
  h' (LiteralBinder l) = h (LiteralBinder (lit h' l))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' (PositionedBinder pos com b) = h (PositionedBinder pos com (h' b))
  h' (TypedBinder t b) = h (TypedBinder t (h' b))
  h' other = h other

  lit :: (a -> a) -> Literal a -> Literal a
  lit go (ArrayLiteral as) = ArrayLiteral (map go as)
  lit go (ObjectLiteral as) = ObjectLiteral (map (fmap go) as)
  lit _ other = other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = map (mapGuardedExpr handleGuard g') (caseAlternativeResult ca)
       }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com (handleDoNotationElement e)

  handleGuard :: Guard -> Guard
  handleGuard (ConditionGuard e) = ConditionGuard (g' e)
  handleGuard (PatternGuard b e) = PatternGuard (h' b) (g' e)

everywhereOnValuesTopDownM
  :: forall m
   . (Monad m)
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> ( Declaration -> m Declaration
     , Expr -> m Expr
     , Binder -> m Binder
     )
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where

  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds
  f' (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h' <=< h) bs <*> traverse (guardedExprM handleGuard (g' <=< g)) val
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration name args implies deps ds) = TypeClassDeclaration name args implies deps <$> traverse (f' <=< f) ds
  f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
  f' (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> (f d >>= f')
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal l) = Literal <$> lit (g >=> g') l
  g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) = BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v) = Parens <$> (g v >>= g')
  g' (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g v >>= g')
  g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
  g' (ObjectUpdateNested obj vs) = ObjectUpdateNested <$> (g obj >>= g') <*> traverse (g' <=< g) vs
  g' (Abs name v) = Abs name <$> (g v >>= g')
  g' (App v1 v2) = App <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts) = Case <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let ds v) = Let <$> traverse (f' <=< f) ds <*> (g v >>= g')
  g' (Do es) = Do <$> traverse handleDoNotationElement es
  g' (PositionedValue pos com v) = PositionedValue pos com <$> (g v >>= g')
  g' other = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder l) = LiteralBinder <$> lit (h >=> h') l
  h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h' <=< h) bs
  h' (BinaryNoParensBinder b1 b2 b3) = BinaryNoParensBinder <$> (h b1 >>= h') <*> (h b2 >>= h') <*> (h b3 >>= h')
  h' (ParensInBinder b) = ParensInBinder <$> (h b >>= h')
  h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
  h' (PositionedBinder pos com b) = PositionedBinder pos com <$> (h b >>= h')
  h' (TypedBinder t b) = TypedBinder t <$> (h b >>= h')
  h' other = h other

  lit :: (a -> m a) -> Literal a -> m (Literal a)
  lit go (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM go) as
  lit go (ArrayLiteral as) = ArrayLiteral <$> traverse go as
  lit _ other = pure other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative bs val) =
    CaseAlternative
      <$> traverse (h' <=< h) bs
      <*> traverse (guardedExprM handleGuard (g' <=< g)) val

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse (f' <=< f) ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard e) = ConditionGuard <$> (g' <=< g) e
  handleGuard (PatternGuard b e) = PatternGuard <$> (h' <=< h) b <*> (g' <=< g) e

everywhereOnValuesM
  :: forall m
   . (Monad m)
  => (Declaration -> m Declaration)
  -> (Expr -> m Expr)
  -> (Binder -> m Binder)
  -> ( Declaration -> m Declaration
     , Expr -> m Expr
     , Binder -> m Binder
     )
everywhereOnValuesM f g h = (f', g', h')
  where

  f' :: Declaration -> m Declaration
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
  f' (ValueDeclaration name nameKind bs val) = (ValueDeclaration name nameKind <$> traverse h' bs <*> traverse (guardedExprM handleGuard g') val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> traverse (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration name args implies deps ds) = (TypeClassDeclaration name args implies deps <$> traverse f' ds) >>= f
  f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse f') ds) >>= f
  f' (PositionedDeclaration pos com d) = (PositionedDeclaration pos com <$> f' d) >>= f
  f' other = f other

  g' :: Expr -> m Expr
  g' (Literal l) = (Literal <$> lit g' l) >>= g
  g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) = (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens v) = (Parens <$> g' v) >>= g
  g' (TypeClassDictionaryConstructorApp name v) = (TypeClassDictionaryConstructorApp name <$> g' v) >>= g
  g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> traverse (sndM g') vs) >>= g
  g' (ObjectUpdateNested obj vs) = (ObjectUpdateNested <$> g' obj <*> traverse g' vs) >>= g
  g' (Abs name v) = (Abs name <$> g' v) >>= g
  g' (App v1 v2) = (App <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts) = (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let ds v) = (Let <$> traverse f' ds <*> g' v) >>= g
  g' (Do es) = (Do <$> traverse handleDoNotationElement es) >>= g
  g' (PositionedValue pos com v) = (PositionedValue pos com <$> g' v) >>= g
  g' other = g other

  h' :: Binder -> m Binder
  h' (LiteralBinder l) = (LiteralBinder <$> lit h' l) >>= h
  h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> traverse h' bs) >>= h
  h' (BinaryNoParensBinder b1 b2 b3) = (BinaryNoParensBinder <$> h' b1 <*> h' b2 <*> h' b3) >>= h
  h' (ParensInBinder b) = (ParensInBinder <$> h' b) >>= h
  h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
  h' (PositionedBinder pos com b) = (PositionedBinder pos com <$> h' b) >>= h
  h' (TypedBinder t b) = (TypedBinder t <$> h' b) >>= h
  h' other = h other

  lit :: (a -> m a) -> Literal a -> m (Literal a)
  lit go (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM go) as
  lit go (ArrayLiteral as) = ArrayLiteral <$> traverse go as
  lit _ other = pure other

  handleCaseAlternative :: CaseAlternative -> m CaseAlternative
  handleCaseAlternative (CaseAlternative bs val) =
    CaseAlternative
      <$> traverse h' bs
      <*> traverse (guardedExprM handleGuard g') val

  handleDoNotationElement :: DoNotationElement -> m DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse f' ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

  handleGuard :: Guard -> m Guard
  handleGuard (ConditionGuard e) = ConditionGuard <$> g' e
  handleGuard (PatternGuard b e) = PatternGuard <$> h' b <*> g' e

everythingOnValues
  :: forall r
   . (r -> r -> r)
  -> (Declaration -> r)
  -> (Expr -> r)
  -> (Binder -> r)
  -> (CaseAlternative -> r)
  -> (DoNotationElement -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
     )
everythingOnValues (<>) f g h i j = (f', g', h', i', j')
  where

  f' :: Declaration -> r
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs val) = foldl (<>) (f d) (map h' bs ++ concatMap (\(GuardedExpr grd v) -> map k' grd ++ [g' v]) val)
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ _ d1) = f d <> f' d1
  f' d = f d

  g' :: Expr -> r
  g' v@(Literal l) = lit (g v) g' l
  g' v@(UnaryMinus v1) = g v <> g' v1
  g' v@(BinaryNoParens op v1 v2) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens v1) = g v <> g' v1
  g' v@(TypeClassDictionaryConstructorApp _ v1) = g v <> g' v1
  g' v@(Accessor _ v1) = g v <> g' v1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(ObjectUpdateNested obj vs) = foldl (<>) (g v <> g' obj) (fmap g' vs)
  g' v@(Abs _ v1) = g v <> g' v1
  g' v@(App v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _) = g v <> g' v1
  g' v@(Let ds v1) = foldl (<>) (g v) (map f' ds) <> g' v1
  g' v@(Do es) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ _ v1) = g v <> g' v1
  g' v = g v

  h' :: Binder -> r
  h' b@(LiteralBinder l) = lit (h b) h' l
  h' b@(ConstructorBinder _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(BinaryNoParensBinder b1 b2 b3) = h b <> h' b1 <> h' b2 <> h' b3
  h' b@(ParensInBinder b1) = h b <> h' b1
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ _ b1) = h b <> h' b1
  h' b@(TypedBinder _ b1) = h b <> h' b1
  h' b = h b

  lit :: r -> (a -> r) -> Literal a -> r
  lit r go (ArrayLiteral as) = foldl (<>) r (map go as)
  lit r go (ObjectLiteral as) = foldl (<>) r (map (go . snd) as)
  lit r _ _ = r

  i' :: CaseAlternative -> r
  i' ca@(CaseAlternative bs gs) =
    foldl (<>) (i ca) (map h' bs ++ concatMap (\(GuardedExpr grd val) -> map k' grd ++ [g' val]) gs)

  j' :: DoNotationElement -> r
  j' e@(DoNotationValue v) = j e <> g' v
  j' e@(DoNotationBind b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet ds) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ _ e1) = j e <> j' e1

  k' :: Guard -> r
  k' (ConditionGuard e) = g' e
  k' (PatternGuard b e) = h' b <> g' e

everythingWithContextOnValues
  :: forall s r
   . s
  -> r
  -> (r -> r -> r)
  -> (s -> Declaration       -> (s, r))
  -> (s -> Expr              -> (s, r))
  -> (s -> Binder            -> (s, r))
  -> (s -> CaseAlternative   -> (s, r))
  -> (s -> DoNotationElement -> (s, r))
  -> ( Declaration       -> r
     , Expr              -> r
     , Binder            -> r
     , CaseAlternative   -> r
     , DoNotationElement -> r)
everythingWithContextOnValues s0 r0 (<>) f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where

  f'' :: s -> Declaration -> r
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' :: s -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs val) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(GuardedExpr grd v) -> map (k' s) grd ++ [g'' s v]) val)
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (map (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ _ d1) = f'' s d1
  f' _ _ = r0

  g'' :: s -> Expr -> r
  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' :: s -> Expr -> r
  g' s (Literal l) = lit g'' s l
  g' s (UnaryMinus v1) = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
  g' s (Accessor _ v1) = g'' s v1
  g' s (ObjectUpdate obj vs) = foldl (<>) (g'' s obj) (map (g'' s . snd) vs)
  g' s (ObjectUpdateNested obj vs) = foldl (<>) (g'' s obj) (fmap (g'' s) vs)
  g' s (Abs _ v1) = g'' s v1
  g' s (App v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _) = g'' s v1
  g' s (Let ds v1) = foldl (<>) r0 (map (f'' s) ds) <> g'' s v1
  g' s (Do es) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _ = r0

  h'' :: s -> Binder -> r
  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' :: s -> Binder -> r
  h' s (LiteralBinder l) = lit h'' s l
  h' s (ConstructorBinder _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (BinaryNoParensBinder b1 b2 b3) = h'' s b1 <> h'' s b2 <> h'' s b3
  h' s (ParensInBinder b) = h'' s b
  h' s (NamedBinder _ b1) = h'' s b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1) = h'' s b1
  h' _ _ = r0

  lit :: (s -> a -> r) -> s -> Literal a -> r
  lit go s (ArrayLiteral as) = foldl (<>) r0 (map (go s) as)
  lit go s (ObjectLiteral as) = foldl (<>) r0 (map (go s . snd) as)
  lit _ _ _ = r0

  i'' :: s -> CaseAlternative -> r
  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' :: s -> CaseAlternative -> r
  i' s (CaseAlternative bs gs) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(GuardedExpr grd val) -> map (k' s) grd ++ [g'' s val]) gs)

  j'' :: s -> DoNotationElement -> r
  j'' s e = let (s', r) = j s e in r <> j' s' e

  j' :: s -> DoNotationElement -> r
  j' s (DoNotationValue v) = g'' s v
  j' s (DoNotationBind b v) = h'' s b <> g'' s v
  j' s (DoNotationLet ds) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

  k' :: s -> Guard -> r
  k' s (ConditionGuard e) = g'' s e
  k' s (PatternGuard b e) = h'' s b <> g'' s e

everywhereWithContextOnValuesM
  :: forall m s
   . (Monad m)
  => s
  -> (s -> Declaration       -> m (s, Declaration))
  -> (s -> Expr              -> m (s, Expr))
  -> (s -> Binder            -> m (s, Binder))
  -> (s -> CaseAlternative   -> m (s, CaseAlternative))
  -> (s -> DoNotationElement -> m (s, DoNotationElement))
  -> ( Declaration       -> m Declaration
     , Expr              -> m Expr
     , Binder            -> m Binder
     , CaseAlternative   -> m CaseAlternative
     , DoNotationElement -> m DoNotationElement
     )
everywhereWithContextOnValuesM s0 f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f'' s) ds
  f' s (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> traverse (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration name args implies deps ds) = TypeClassDeclaration name args implies deps <$> traverse (f'' s) ds
  f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds
  f' s (PositionedDeclaration pos com d1) = PositionedDeclaration pos com <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (Literal l) = Literal <$> lit g'' s l
  g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
  g' s (BinaryNoParens op v1 v2) = BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens v) = Parens <$> g'' s v
  g' s (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> g'' s v
  g' s (Accessor prop v) = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> traverse (sndM (g'' s)) vs
  g' s (ObjectUpdateNested obj vs) = ObjectUpdateNested <$> g'' s obj <*> traverse (g'' s) vs
  g' s (Abs name v) = Abs name <$> g'' s v
  g' s (App v1 v2) = App <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts) = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let ds v) = Let <$> traverse (f'' s) ds <*> g'' s v
  g' s (Do es) = Do <$> traverse (j'' s) es
  g' s (PositionedValue pos com v) = PositionedValue pos com <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (LiteralBinder l) = LiteralBinder <$> lit h'' s l
  h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h'' s) bs
  h' s (BinaryNoParensBinder b1 b2 b3) = BinaryNoParensBinder <$> h'' s b1 <*> h'' s b2 <*> h'' s b3
  h' s (ParensInBinder b) = ParensInBinder <$> h'' s b
  h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
  h' s (PositionedBinder pos com b) = PositionedBinder pos com <$> h'' s b
  h' s (TypedBinder t b) = TypedBinder t <$> h'' s b
  h' _ other = return other

  lit :: (s -> a -> m a) -> s -> Literal a -> m (Literal a)
  lit go s (ArrayLiteral as) = ArrayLiteral <$> traverse (go s) as
  lit go s (ObjectLiteral as) = ObjectLiteral <$> traverse (sndM (go s)) as
  lit _ _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs val) = CaseAlternative <$> traverse (h'' s) bs <*> traverse (guardedExprM (k' s) (g'' s)) val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue v) = DoNotationValue <$> g'' s v
  j' s (DoNotationBind b v) = DoNotationBind <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ds) = DoNotationLet <$> traverse (f'' s) ds
  j' s (PositionedDoNotationElement pos com e1) = PositionedDoNotationElement pos com <$> j'' s e1

  k' s (ConditionGuard e) = ConditionGuard <$> g'' s e
  k' s (PatternGuard b e) = PatternGuard <$> h'' s b <*> g'' s e

everythingWithScope
  :: forall r
   . (Monoid r)
  => (S.Set Ident -> Declaration -> r)
  -> (S.Set Ident -> Expr -> r)
  -> (S.Set Ident -> Binder -> r)
  -> (S.Set Ident -> CaseAlternative -> r)
  -> (S.Set Ident -> DoNotationElement -> r)
  -> ( S.Set Ident -> Declaration -> r
     , S.Set Ident -> Expr -> r
     , S.Set Ident -> Binder -> r
     , S.Set Ident -> CaseAlternative -> r
     , S.Set Ident -> DoNotationElement -> r
     )
everythingWithScope f g h i j = (f'', g'', h'', i'', \s -> snd . j'' s)
  where
  -- Avoid importing Data.Monoid and getting shadowed names above
  (<>) = mappend

  f'' :: S.Set Ident -> Declaration -> r
  f'' s a = f s a <> f' s a

  f' :: S.Set Ident -> Declaration -> r
  f' s (DataBindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds
  f' s (ValueDeclaration name _ bs val) =
    let s' = S.insert name s
        s'' = S.union s' (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s') bs <> foldMap (l' s'') val
  f' s (BindingGroupDeclaration ds) =
    let s' = S.union s (S.fromList (map (\(name, _, _) -> name) ds))
    in foldMap (\(_, _, val) -> g'' s' val) ds
  f' s (TypeClassDeclaration _ _ _ _ ds) = foldMap (f'' s) ds
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
  f' s (PositionedDeclaration _ _ d) = f'' s d
  f' _ _ = mempty

  g'' :: S.Set Ident -> Expr -> r
  g'' s a = g s a <> g' s a

  g' :: S.Set Ident -> Expr -> r
  g' s (Literal l) = lit g'' s l
  g' s (UnaryMinus v1) = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1) = g'' s v1
  g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
  g' s (Accessor _ v1) = g'' s v1
  g' s (ObjectUpdate obj vs) = g'' s obj <> foldMap (g'' s . snd) vs
  g' s (ObjectUpdateNested obj vs) = g'' s obj <> foldMap (g'' s) vs
  g' s (Abs (Left name) v1) =
    let s' = S.insert name s
    in g'' s' v1
  g' s (Abs (Right b) v1) =
    let s' = S.union (S.fromList (binderNames b)) s
    in g'' s' v1
  g' s (App v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts) = foldMap (g'' s) vs <> foldMap (i'' s) alts
  g' s (TypedValue _ v1 _) = g'' s v1
  g' s (Let ds v1) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in foldMap (f'' s') ds <> g'' s' v1
  g' s (Do es) = fold . snd . mapAccumL j'' s $ es
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _ = mempty

  h'' :: S.Set Ident -> Binder -> r
  h'' s a = h s a <> h' s a

  h' :: S.Set Ident -> Binder -> r
  h' s (LiteralBinder l) = lit h'' s l
  h' s (ConstructorBinder _ bs) = foldMap (h'' s) bs
  h' s (BinaryNoParensBinder b1 b2 b3) = foldMap (h'' s) [b1, b2, b3]
  h' s (ParensInBinder b) = h'' s b
  h' s (NamedBinder name b1) = h'' (S.insert name s) b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1) = h'' s b1
  h' _ _ = mempty

  lit :: (S.Set Ident -> a -> r) -> S.Set Ident -> Literal a -> r
  lit go s (ArrayLiteral as) = foldMap (go s) as
  lit go s (ObjectLiteral as) = foldMap (go s . snd) as
  lit _ _ _ = mempty

  i'' :: S.Set Ident -> CaseAlternative -> r
  i'' s a = i s a <> i' s a

  i' :: S.Set Ident -> CaseAlternative -> r
  i' s (CaseAlternative bs gs) =
    let s' = S.union s (S.fromList (concatMap binderNames bs))
    in foldMap (h'' s) bs <> foldMap (l' s') gs

  j'' :: S.Set Ident -> DoNotationElement -> (S.Set Ident, r)
  j'' s a = let (s', r) = j' s a in (s', j s a <> r)

  j' :: S.Set Ident -> DoNotationElement -> (S.Set Ident, r)
  j' s (DoNotationValue v) = (s, g'' s v)
  j' s (DoNotationBind b v) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s v)
  j' s (DoNotationLet ds) =
    let s' = S.union s (S.fromList (mapMaybe getDeclIdent ds))
    in (s', foldMap (f'' s') ds)
  j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

  k' :: S.Set Ident -> Guard -> (S.Set Ident, r)
  k' s (ConditionGuard e) = (s, g'' s e)
  k' s (PatternGuard b e) =
    let s' = S.union (S.fromList (binderNames b)) s
    in (s', h'' s b <> g'' s' e)

  l' s (GuardedExpr [] e) = g'' s e
  l' s (GuardedExpr (grd:gs) e) =
    let (s', r) = k' s grd
    in r <> l' s' (GuardedExpr gs e)

  getDeclIdent :: Declaration -> Maybe Ident
  getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
  getDeclIdent (ValueDeclaration ident _ _ _) = Just ident
  getDeclIdent (TypeDeclaration ident _) = Just ident
  getDeclIdent _ = Nothing

accumTypes
  :: (Monoid r)
  => (Type -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
     )
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ dctors) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ ty) = f ty
  forDecls (TypeClassDeclaration _ _ implies _ _) = mconcat (concatMap (map f . constraintArgs) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f . constraintArgs) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty) = f ty
  forDecls (TypeDeclaration _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary c _ _) = mconcat (map f (constraintArgs c))
  forValues (DeferredDictionary _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ ty) = f ty
  forValues _ = mempty

accumKinds
  :: (Monoid r)
  => (Kind -> r)
  -> ( Declaration -> r
     , Expr -> r
     , Binder -> r
     , CaseAlternative -> r
     , DoNotationElement -> r
     )
accumKinds f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ args dctors) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . snd) dctors
  forDecls (TypeClassDeclaration _ args implies _ _) =
    foldMap (foldMap f . snd) args `mappend`
    foldMap (foldMap forTypes . constraintArgs) implies
  forDecls (TypeInstanceDeclaration _ cs _ tys _) =
    foldMap (foldMap forTypes . constraintArgs) cs `mappend`
    foldMap forTypes tys
  forDecls (TypeSynonymDeclaration _ args ty) =
    foldMap (foldMap f . snd) args `mappend`
    forTypes ty
  forDecls (TypeDeclaration _ ty) = forTypes ty
  forDecls (ExternDeclaration _ ty) = forTypes ty
  forDecls (ExternDataDeclaration _ kn) = f kn
  forDecls _ = mempty

  forValues (TypeClassDictionary c _ _) = foldMap forTypes (constraintArgs c)
  forValues (DeferredDictionary _ tys) = foldMap forTypes tys
  forValues (TypedValue _ _ ty) = forTypes ty
  forValues _ = mempty

  forTypes (KindedType _ k) = f k
  forTypes _ = mempty

-- |
-- Map a function over type annotations appearing inside a value
--
overTypes :: (Type -> Type) -> Expr -> Expr
overTypes f = let (_, f', _) = everywhereOnValues id g id in f'
  where
  g :: Expr -> Expr
  g (TypedValue checkTy val t) = TypedValue checkTy val (f t)
  g (TypeClassDictionary c sco hints) = TypeClassDictionary (mapConstraintArgs (map f) c) sco hints
  g other = other
