
module TypeClassWithFunDeps where

class TypeClassWithFunDeps a b c d e | a b -> c, c -> d e where
  aMember :: a
