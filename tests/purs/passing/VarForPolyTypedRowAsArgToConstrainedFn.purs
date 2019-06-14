module Main where

import Prelude (Unit, identity)
import Effect (Effect)
import Effect.Console (log)
import Type.RowList (class RowToList, Cons, RLProxy(RLProxy), kind RowList)

class GId (l :: RowList) (r :: # Type) | l -> r where
  gId :: RLProxy l -> Record r -> Record r

instance gIdCons :: GId (Cons s (va -> vb) l') r where
  gId l record = record

id :: forall l r. GId l r => RowToList r l => Record r -> Record r
id = gId (RLProxy :: RLProxy l)

x0 :: { a :: forall a. a -> a }
x0 = { a: (identity :: forall a. a -> a) }

x1 = id x0

main :: Effect Unit
main = log "Done"
