module Main where

import Prelude (class Functor, Unit, identity)
import Effect (Effect)
import Effect.Console (log)
import Type.RowList (class RowToList, Cons, RLProxy(RLProxy), kind RowList)

class GId (l :: RowList) (r :: # Type) | l -> r where
  gId :: RLProxy l -> Record r -> Record r

instance gIdCons :: GId (Cons s (va -> vb) l') r where
  gId l record = record

id :: forall l r. GId l r => RowToList r l => Record r -> Record r
id = gId (RLProxy :: RLProxy l)

x1 = id { a: (identity :: forall a. a -> a) }
x2 = id ({ a: identity } :: { a :: forall a. a -> a })
x3 = id ({ a: (identity :: forall a. a -> a) } :: { a :: forall a. a -> a })
x4 = id { a: (identity :: forall a f. Functor f => f a -> f a) }

main :: Effect Unit
main = log "Done"
