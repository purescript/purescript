module Data.Record where

class RowUnion (l :: # *) (r :: # *) (o :: # *) | l r -> o

foreign import merge :: forall a b c. RowUnion a b c => {|a} -> {|b} -> {|c}
