module Main where

  type Lens a b = 
    { get :: a -> b
    , set :: a -> b -> a
    }

  composeLenses :: forall a b c. Lens a b -> Lens b c -> Lens a c
  composeLenses = \l1 -> \l2 ->
    { get: \a -> l2.get (l1.get a)
    , set: \a c -> l1.set a (l2.set (l1.get a) c)
    }

  type Pair a b = { fst :: a, snd :: b }

  fst :: forall a b. Lens (Pair a b) a
  fst = 
    { get: \p -> p.fst
    , set: \p a -> { fst: a, snd: p.snd }
    }

  test1 :: forall a b c. Lens (Pair (Pair a b) c) a
  test1 = composeLenses fst fst

  -- Test partially applied nested synonyms
  type F x y = x -> y

  type G x = F x

  f :: G String String -> String
  f k = k "Done"

  main = Debug.Trace.trace (f id)
