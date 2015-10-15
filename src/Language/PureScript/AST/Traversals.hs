-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.Traversals
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | AST traversal helpers
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.PureScript.AST.Traversals where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..), mconcat)
#endif
import Data.Maybe (mapMaybe)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Arrow ((***), (+++), second)

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Declarations
import Language.PureScript.Types
import Language.PureScript.Traversals

everywhereOnValues :: (Declaration -> Declaration) ->
                      (Expr -> Expr) ->
                      (Binder -> Binder) ->
                      (Declaration -> Declaration, Expr -> Expr, Binder -> Binder)
everywhereOnValues f g h = (f', g', h')
  where
  f' :: Declaration -> Declaration
  f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
  f' (ValueDeclaration name nameKind bs val) = f (ValueDeclaration name nameKind (map h' bs) ((map (g' *** g') +++ g') val))
  f' (BindingGroupDeclaration ds) = f (BindingGroupDeclaration (map (\(name, nameKind, val) -> (name, nameKind, g' val)) ds))
  f' (TypeClassDeclaration name args implies ds) = f (TypeClassDeclaration name args implies (map f' ds))
  f' (TypeInstanceDeclaration name cs className args ds) = f (TypeInstanceDeclaration name cs className args (mapTypeInstanceBody (map f') ds))
  f' (PositionedDeclaration pos com d) = f (PositionedDeclaration pos com (f' d))
  f' other = f other

  g' :: Expr -> Expr
  g' (UnaryMinus v) = g (UnaryMinus (g' v))
  g' (BinaryNoParens op v1 v2) = g (BinaryNoParens (g' op) (g' v1) (g' v2))
  g' (Parens v) = g (Parens (g' v))
  g' (OperatorSection op (Left v)) = g (OperatorSection (g' op) (Left $ g' v))
  g' (OperatorSection op (Right v)) = g (OperatorSection (g' op) (Right $ g' v))
  g' (ArrayLiteral vs) = g (ArrayLiteral (map g' vs))
  g' (ObjectLiteral vs) = g (ObjectLiteral (map (fmap g') vs))
  g' (ObjectConstructor vs) = g (ObjectConstructor (map (second (fmap g')) vs))
  g' (TypeClassDictionaryConstructorApp name v) = g (TypeClassDictionaryConstructorApp name (g' v))
  g' (Accessor prop v) = g (Accessor prop (g' v))
  g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (fmap g') vs))
  g' (ObjectUpdater obj vs) = g (ObjectUpdater (fmap g' obj) (map (second (fmap g')) vs))
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
  h' (ObjectBinder bs) = h (ObjectBinder (map (fmap h') bs))
  h' (ArrayBinder bs) = h (ArrayBinder (map h' bs))
  h' (NamedBinder name b) = h (NamedBinder name (h' b))
  h' (PositionedBinder pos com b) = h (PositionedBinder pos com (h' b))
  h' (TypedBinder t b) = h (TypedBinder t (h' b))
  h' other = h other

  handleCaseAlternative :: CaseAlternative -> CaseAlternative
  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

  handleDoNotationElement :: DoNotationElement -> DoNotationElement
  handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com (handleDoNotationElement e)

everywhereOnValuesTopDownM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Expr -> m Expr) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Expr -> m Expr, Binder -> m Binder)
everywhereOnValuesTopDownM f g h = (f' <=< f, g' <=< g, h' <=< h)
  where
  f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> mapM (f' <=< f) ds
  f' (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> mapM (h' <=< h) bs <*> eitherM (mapM (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val
  f' (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> mapM (\(name, nameKind, val) -> (,,) name nameKind <$> (g val >>= g')) ds
  f' (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> mapM (f' <=< f) ds
  f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
  f' (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> (f d >>= f')
  f' other = f other

  g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
  g' (BinaryNoParens op v1 v2) = BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
  g' (Parens v) = Parens <$> (g v >>= g')
  g' (OperatorSection op (Left v)) = OperatorSection <$> (g op >>= g') <*> (Left <$> (g v >>= g'))
  g' (OperatorSection op (Right v)) = OperatorSection <$> (g op >>= g') <*> (Right <$> (g v >>= g'))
  g' (ArrayLiteral vs) = ArrayLiteral <$> mapM (g' <=< g) vs
  g' (ObjectLiteral vs) = ObjectLiteral <$> mapM (sndM (g' <=< g)) vs
  g' (ObjectConstructor vs) = ObjectConstructor <$> mapM (sndM $ maybeM (g' <=< g)) vs
  g' (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g v >>= g')
  g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
  g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> mapM (sndM (g' <=< g)) vs
  g' (ObjectUpdater obj vs) = ObjectUpdater <$> (maybeM g obj >>= maybeM g') <*> mapM (sndM $ maybeM (g' <=< g)) vs
  g' (Abs name v) = Abs name <$> (g v >>= g')
  g' (App v1 v2) = App <$> (g v1 >>= g') <*> (g v2 >>= g')
  g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
  g' (Case vs alts) = Case <$> mapM (g' <=< g) vs <*> mapM handleCaseAlternative alts
  g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
  g' (Let ds v) = Let <$> mapM (f' <=< f) ds <*> (g v >>= g')
  g' (Do es) = Do <$> mapM handleDoNotationElement es
  g' (PositionedValue pos com v) = PositionedValue pos com <$> (g v >>= g')
  g' other = g other

  h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> mapM (h' <=< h) bs
  h' (ObjectBinder bs) = ObjectBinder <$> mapM (sndM (h' <=< h)) bs
  h' (ArrayBinder bs) = ArrayBinder <$> mapM (h' <=< h) bs
  h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
  h' (PositionedBinder pos com b) = PositionedBinder pos com <$> (h b >>= h')
  h' (TypedBinder t b) = TypedBinder t <$> (h b >>= h')
  h' other = h other

  handleCaseAlternative (CaseAlternative bs val) = CaseAlternative <$> mapM (h' <=< h) bs
                                                                   <*> eitherM (mapM (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g' <=< g) v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> mapM (f' <=< f) ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

everywhereOnValuesM :: (Functor m, Applicative m, Monad m) =>
  (Declaration -> m Declaration) ->
  (Expr -> m Expr) ->
  (Binder -> m Binder) ->
  (Declaration -> m Declaration, Expr -> m Expr, Binder -> m Binder)
everywhereOnValuesM f g h = (f', g', h')
  where
  f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> mapM f' ds) >>= f
  f' (ValueDeclaration name nameKind bs val) = (ValueDeclaration name nameKind <$> mapM h' bs <*> eitherM (mapM (pairM g' g')) g' val) >>= f
  f' (BindingGroupDeclaration ds) = (BindingGroupDeclaration <$> mapM (\(name, nameKind, val) -> (,,) name nameKind <$> g' val) ds) >>= f
  f' (TypeClassDeclaration name args implies ds) = (TypeClassDeclaration name args implies <$> mapM f' ds) >>= f
  f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (mapM f') ds) >>= f
  f' (PositionedDeclaration pos com d) = (PositionedDeclaration pos com <$> f' d) >>= f
  f' other = f other

  g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
  g' (BinaryNoParens op v1 v2) = (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2) >>= g
  g' (Parens v) = (Parens <$> g' v) >>= g
  g' (OperatorSection op (Left v)) = (OperatorSection <$> g' op <*> (Left <$> g' v)) >>= g
  g' (OperatorSection op (Right v)) = (OperatorSection <$> g' op <*> (Right <$> g' v)) >>= g
  g' (ArrayLiteral vs) = (ArrayLiteral <$> mapM g' vs) >>= g
  g' (ObjectLiteral vs) = (ObjectLiteral <$> mapM (sndM g') vs) >>= g
  g' (ObjectConstructor vs) = (ObjectConstructor <$> mapM (sndM $ maybeM g') vs) >>= g
  g' (TypeClassDictionaryConstructorApp name v) = (TypeClassDictionaryConstructorApp name <$> g' v) >>= g
  g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
  g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> mapM (sndM g') vs) >>= g
  g' (ObjectUpdater obj vs) = (ObjectUpdater <$> maybeM g' obj <*> mapM (sndM $ maybeM g') vs) >>= g
  g' (Abs name v) = (Abs name <$> g' v) >>= g
  g' (App v1 v2) = (App <$> g' v1 <*> g' v2) >>= g
  g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
  g' (Case vs alts) = (Case <$> mapM g' vs <*> mapM handleCaseAlternative alts) >>= g
  g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
  g' (Let ds v) = (Let <$> mapM f' ds <*> g' v) >>= g
  g' (Do es) = (Do <$> mapM handleDoNotationElement es) >>= g
  g' (PositionedValue pos com v) = (PositionedValue pos com <$> g' v) >>= g
  g' other = g other

  h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> mapM h' bs) >>= h
  h' (ObjectBinder bs) = (ObjectBinder <$> mapM (sndM h') bs) >>= h
  h' (ArrayBinder bs) = (ArrayBinder <$> mapM h' bs) >>= h
  h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
  h' (PositionedBinder pos com b) = (PositionedBinder pos com <$> h' b) >>= h
  h' (TypedBinder t b) = (TypedBinder t <$> h' b) >>= h
  h' other = h other

  handleCaseAlternative (CaseAlternative bs val) = CaseAlternative <$> mapM h' bs
                                                                   <*> eitherM (mapM (pairM g' g')) g' val

  handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
  handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
  handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> mapM f' ds
  handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

everythingOnValues :: (r -> r -> r) ->
                      (Declaration -> r) ->
                      (Expr -> r) ->
                      (Binder -> r) ->
                      (CaseAlternative -> r) ->
                      (DoNotationElement -> r) ->
                      (Declaration -> r, Expr -> r, Binder -> r, CaseAlternative -> r, DoNotationElement -> r)
everythingOnValues (<>) f g h i j = (f', g', h', i', j')
  where
  f' d@(DataBindingGroupDeclaration ds) = foldl (<>) (f d) (map f' ds)
  f' d@(ValueDeclaration _ _ bs (Right val)) = foldl (<>) (f d) (map h' bs) <> g' val
  f' d@(ValueDeclaration _ _ bs (Left gs)) = foldl (<>) (f d) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)
  f' d@(BindingGroupDeclaration ds) = foldl (<>) (f d) (map (\(_, _, val) -> g' val) ds)
  f' d@(TypeClassDeclaration _ _ _ ds) = foldl (<>) (f d) (map f' ds)
  f' d@(TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) (f d) (map f' ds)
  f' d@(PositionedDeclaration _ _ d1) = f d <> f' d1
  f' d = f d

  g' v@(UnaryMinus v1) = g v <> g' v1
  g' v@(BinaryNoParens op v1 v2) = g v <> g' op <> g' v1 <> g' v2
  g' v@(Parens v1) = g v <> g' v1
  g' v@(OperatorSection op (Left v1)) = g v <> g' op <> g' v1
  g' v@(OperatorSection op (Right v1)) = g v <> g' op <> g' v1
  g' v@(ArrayLiteral vs) = foldl (<>) (g v) (map g' vs)
  g' v@(ObjectLiteral vs) = foldl (<>) (g v) (map (g' . snd) vs)
  g' v@(ObjectConstructor vs) = foldl (<>) (g v) (map g' (mapMaybe snd vs))
  g' v@(TypeClassDictionaryConstructorApp _ v1) = g v <> g' v1
  g' v@(Accessor _ v1) = g v <> g' v1
  g' v@(ObjectUpdate obj vs) = foldl (<>) (g v <> g' obj) (map (g' . snd) vs)
  g' v@(ObjectUpdater obj vs) = foldl (<>) (maybe (g v) (\x -> g v <> g' x) obj) (map g' (mapMaybe snd vs))
  g' v@(Abs _ v1) = g v <> g' v1
  g' v@(App v1 v2) = g v <> g' v1 <> g' v2
  g' v@(IfThenElse v1 v2 v3) = g v <> g' v1 <> g' v2 <> g' v3
  g' v@(Case vs alts) = foldl (<>) (foldl (<>) (g v) (map g' vs)) (map i' alts)
  g' v@(TypedValue _ v1 _) = g v <> g' v1
  g' v@(Let ds v1) = foldl (<>) (g v) (map f' ds) <> g' v1
  g' v@(Do es) = foldl (<>) (g v) (map j' es)
  g' v@(PositionedValue _ _ v1) = g v <> g' v1
  g' v = g v

  h' b@(ConstructorBinder _ bs) = foldl (<>) (h b) (map h' bs)
  h' b@(ObjectBinder bs) = foldl (<>) (h b) (map (h' . snd) bs)
  h' b@(ArrayBinder bs) = foldl (<>) (h b) (map h' bs)
  h' b@(NamedBinder _ b1) = h b <> h' b1
  h' b@(PositionedBinder _ _ b1) = h b <> h' b1
  h' b@(TypedBinder _ b1) = h b <> h' b1
  h' b = h b

  i' ca@(CaseAlternative bs (Right val)) = foldl (<>) (i ca) (map h' bs) <> g' val
  i' ca@(CaseAlternative bs (Left gs)) = foldl (<>) (i ca) (map h' bs ++ concatMap (\(grd, val) -> [g' grd, g' val]) gs)

  j' e@(DoNotationValue v) = j e <> g' v
  j' e@(DoNotationBind b v) = j e <> h' b <> g' v
  j' e@(DoNotationLet ds) = foldl (<>) (j e) (map f' ds)
  j' e@(PositionedDoNotationElement _ _ e1) = j e <> j' e1

everythingWithContextOnValues ::
  s ->
  r ->
  (r -> r -> r) ->
  (s -> Declaration       -> (s, r)) ->
  (s -> Expr              -> (s, r)) ->
  (s -> Binder            -> (s, r)) ->
  (s -> CaseAlternative   -> (s, r)) ->
  (s -> DoNotationElement -> (s, r)) ->
  ( Declaration       -> r
  , Expr              -> r
  , Binder            -> r
  , CaseAlternative   -> r
  , DoNotationElement -> r)
everythingWithContextOnValues s0 r0 (<>) f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s d = let (s', r) = f s d in r <> f' s' d

  f' s (DataBindingGroupDeclaration ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (ValueDeclaration _ _ bs (Right val)) = foldl (<>) r0 (map (h'' s) bs) <> g'' s val
  f' s (ValueDeclaration _ _ bs (Left gs)) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(grd, val) -> [g'' s grd, g'' s val]) gs)
  f' s (BindingGroupDeclaration ds) = foldl (<>) r0 (map (\(_, _, val) -> g'' s val) ds)
  f' s (TypeClassDeclaration _ _ _ ds) = foldl (<>) r0 (map (f'' s) ds)
  f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl (<>) r0 (map (f'' s) ds)
  f' s (PositionedDeclaration _ _ d1) = f'' s d1
  f' _ _ = r0

  g'' s v = let (s', r) = g s v in r <> g' s' v

  g' s (UnaryMinus v1) = g'' s v1
  g' s (BinaryNoParens op v1 v2) = g'' s op <> g'' s v1 <> g'' s v2
  g' s (Parens v1) = g'' s v1
  g' s (OperatorSection op (Left v)) = g'' s op <> g'' s v
  g' s (OperatorSection op (Right v)) = g'' s op <> g'' s v
  g' s (ArrayLiteral vs) = foldl (<>) r0 (map (g'' s) vs)
  g' s (ObjectLiteral vs) = foldl (<>) r0 (map (g'' s . snd) vs)
  g' s (ObjectConstructor vs) = foldl (<>) r0 (map (g'' s) (mapMaybe snd vs))
  g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
  g' s (Accessor _ v1) = g'' s v1
  g' s (ObjectUpdate obj vs) = foldl (<>) (g'' s obj) (map (g'' s . snd) vs)
  g' s (ObjectUpdater obj vs) = foldl (<>) (maybe r0 (g'' s) obj) (map (g'' s) (mapMaybe snd vs))
  g' s (Abs _ v1) = g'' s v1
  g' s (App v1 v2) = g'' s v1 <> g'' s v2
  g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
  g' s (Case vs alts) = foldl (<>) (foldl (<>) r0 (map (g'' s) vs)) (map (i'' s) alts)
  g' s (TypedValue _ v1 _) = g'' s v1
  g' s (Let ds v1) = foldl (<>) r0 (map (f'' s) ds) <> g'' s v1
  g' s (Do es) = foldl (<>) r0 (map (j'' s) es)
  g' s (PositionedValue _ _ v1) = g'' s v1
  g' _ _ = r0

  h'' s b = let (s', r) = h s b in r <> h' s' b

  h' s (ConstructorBinder _ bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (ObjectBinder bs) = foldl (<>) r0 (map (h'' s . snd) bs)
  h' s (ArrayBinder bs) = foldl (<>) r0 (map (h'' s) bs)
  h' s (NamedBinder _ b1) = h'' s b1
  h' s (PositionedBinder _ _ b1) = h'' s b1
  h' s (TypedBinder _ b1) = h'' s b1
  h' _ _ = r0

  i'' s ca = let (s', r) = i s ca in r <> i' s' ca

  i' s (CaseAlternative bs (Right val)) = foldl (<>) r0 (map (h'' s) bs) <> g'' s val
  i' s (CaseAlternative bs (Left gs)) = foldl (<>) r0 (map (h'' s) bs ++ concatMap (\(grd, val) -> [g'' s grd, g'' s val]) gs)

  j'' s e = let (s', r) = j s e in r <> j' s' e

  j' s (DoNotationValue v) = g'' s v
  j' s (DoNotationBind b v) = h'' s b <> g'' s v
  j' s (DoNotationLet ds) = foldl (<>) r0 (map (f'' s) ds)
  j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

everywhereWithContextOnValuesM :: (Functor m, Applicative m, Monad m) =>
  s ->
  (s -> Declaration       -> m (s, Declaration)) ->
  (s -> Expr              -> m (s, Expr)) ->
  (s -> Binder            -> m (s, Binder)) ->
  (s -> CaseAlternative   -> m (s, CaseAlternative)) ->
  (s -> DoNotationElement -> m (s, DoNotationElement)) ->
  ( Declaration       -> m Declaration
  , Expr              -> m Expr
  , Binder            -> m Binder
  , CaseAlternative   -> m CaseAlternative
  , DoNotationElement -> m DoNotationElement)
everywhereWithContextOnValuesM s0 f g h i j = (f'' s0, g'' s0, h'' s0, i'' s0, j'' s0)
  where
  f'' s = uncurry f' <=< f s

  f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> mapM (f'' s) ds
  f' s (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> mapM (h'' s) bs <*> eitherM (mapM (pairM (g'' s) (g'' s))) (g'' s) val
  f' s (BindingGroupDeclaration ds) = BindingGroupDeclaration <$> mapM (thirdM (g'' s)) ds
  f' s (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> mapM (f'' s) ds
  f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (mapM (f'' s)) ds
  f' s (PositionedDeclaration pos com d1) = PositionedDeclaration pos com <$> f'' s d1
  f' _ other = return other

  g'' s = uncurry g' <=< g s

  g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
  g' s (BinaryNoParens op v1 v2) = BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2
  g' s (Parens v) = Parens <$> g'' s v
  g' s (OperatorSection op (Left v)) = OperatorSection <$> g'' s op <*> (Left <$> g'' s v)
  g' s (OperatorSection op (Right v)) = OperatorSection <$> g'' s op <*> (Right <$> g'' s v)
  g' s (ArrayLiteral vs) = ArrayLiteral <$> mapM (g'' s) vs
  g' s (ObjectLiteral vs) = ObjectLiteral <$> mapM (sndM (g'' s)) vs
  g' s (ObjectConstructor vs) = ObjectConstructor <$> mapM (sndM $ maybeM (g'' s)) vs
  g' s (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> g'' s v
  g' s (Accessor prop v) = Accessor prop <$> g'' s v
  g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> mapM (sndM (g'' s)) vs
  g' s (ObjectUpdater obj vs) = ObjectUpdater <$> maybeM (g'' s) obj <*> mapM (sndM $ maybeM (g'' s)) vs
  g' s (Abs name v) = Abs name <$> g'' s v
  g' s (App v1 v2) = App <$> g'' s v1 <*> g'' s v2
  g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
  g' s (Case vs alts) = Case <$> mapM (g'' s) vs <*> mapM (i'' s) alts
  g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
  g' s (Let ds v) = Let <$> mapM (f'' s) ds <*> g'' s v
  g' s (Do es) = Do <$> mapM (j'' s) es
  g' s (PositionedValue pos com v) = PositionedValue pos com <$> g'' s v
  g' _ other = return other

  h'' s = uncurry h' <=< h s

  h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> mapM (h'' s) bs
  h' s (ObjectBinder bs) = ObjectBinder <$> mapM (sndM (h'' s)) bs
  h' s (ArrayBinder bs) = ArrayBinder <$> mapM (h'' s) bs
  h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
  h' s (PositionedBinder pos com b) = PositionedBinder pos com <$> h'' s b
  h' s (TypedBinder t b) = TypedBinder t <$> h'' s b
  h' _ other = return other

  i'' s = uncurry i' <=< i s

  i' s (CaseAlternative bs val) = CaseAlternative <$> mapM (h'' s) bs <*> eitherM (mapM (pairM (g'' s) (g'' s))) (g'' s) val

  j'' s = uncurry j' <=< j s

  j' s (DoNotationValue v) = DoNotationValue <$> g'' s v
  j' s (DoNotationBind b v) = DoNotationBind <$> h'' s b <*> g'' s v
  j' s (DoNotationLet ds) = DoNotationLet <$> mapM (f'' s) ds
  j' s (PositionedDoNotationElement pos com e1) = PositionedDoNotationElement pos com <$> j'' s e1

accumTypes :: (Monoid r) => (Type -> r) -> (Declaration -> r, Expr -> r, Binder -> r, CaseAlternative -> r, DoNotationElement -> r)
accumTypes f = everythingOnValues mappend forDecls forValues (const mempty) (const mempty) (const mempty)
  where
  forDecls (DataDeclaration _ _ _ dctors) = mconcat (concatMap (map f . snd) dctors)
  forDecls (ExternDeclaration _ ty) = f ty
  forDecls (TypeClassDeclaration _ _ implies _) = mconcat (concatMap (map f . snd) implies)
  forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f . snd) cs) `mappend` mconcat (map f tys)
  forDecls (TypeSynonymDeclaration _ _ ty) = f ty
  forDecls (TypeDeclaration _ ty) = f ty
  forDecls _ = mempty

  forValues (TypeClassDictionary (_, cs) _) = mconcat (map f cs)
  forValues (SuperClassDictionary _ tys) = mconcat (map f tys)
  forValues (TypedValue _ _ ty) = f ty
  forValues _ = mempty
