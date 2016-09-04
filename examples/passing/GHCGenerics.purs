module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

data Sum a b = Inl a | Inr b

data Ctor (name :: Symbol) a = Ctor a

data K a = K a

class Generic a repr | a -> repr where
  to :: a -> repr
  from :: repr -> a

data MyType a = A String | B a

instance genericMyType :: Generic (MyType a) (Sum (Ctor "A" (K String)) (Ctor "B" (K a))) where
  to (A s) = Inl (Ctor (K s))
  to (B a) = Inr (Ctor (K a))
  from (Inl (Ctor (K s))) = A s
  from (Inr (Ctor (K a))) = B a

class GShow a where
  gShow :: a -> String

class KnownSymbol (sym :: Symbol) where
  symbol :: forall proxy. proxy sym -> String

instance knownSymbolA :: KnownSymbol "A" where
  symbol _ = "A"

instance knownSymbolB :: KnownSymbol "B" where
  symbol _ = "B"

data SProxy (sym :: Symbol) = SProxy

instance gShowSum :: (GShow a, GShow b) => GShow (Sum a b) where
  gShow (Inl a) = gShow a
  gShow (Inr b) = gShow b

instance gShowCtor :: (KnownSymbol ctor, GShow a) => GShow (Ctor ctor a) where
  gShow (Ctor a) = "(" <> symbol (SProxy :: SProxy ctor) <> " " <> gShow a <> ")"

instance gShowK :: Show a => GShow (K a) where
  gShow (K a) = show a

genericShow :: forall a repr. (Generic a repr, GShow repr) => a -> String
genericShow x = gShow (to x)

instance showMyType :: Show a => Show (MyType a) where
  show = genericShow

example :: MyType (MyType Int)
example = B (A "x")

main = do
  logShow (B 1)
  logShow example
  log "Done"
