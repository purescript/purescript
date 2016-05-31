module RebuildSpecWithHiddenIdent (exported) where

hidden x _ = x

exported :: forall a. a -> a
exported x = x
