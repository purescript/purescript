module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Empty = Empty
data Cons = Cons


-- simple case
class Simple a b | a -> b where c :: a -> b
instance simple0 :: Simple Empty {} where c _ = {}
instance simple1 :: Simple Cons {foo :: Cons} where c cons = {foo: cons}


-- simple transitive example
class Transitive a b c | a -> b, b -> c where d :: a -> c
instance transitive :: Transitive Empty {} {} where d _ = {}


-- transitive example with cycles
class Cyclic a b c d | a -> b, b -> a
                     , a -> c
                     , c -> d, d -> c
instance cyclic :: Cyclic Empty Empty {} {}


-- Determined cycle
class DeterminedCycle a b c | a -> b
                            , b -> c, c -> b
instance determinedCycle :: DeterminedCycle Empty {} {}


-- multiple determiners
class MultipleDeterminers a b c d | a b -> c d
instance multipleDeterminers :: MultipleDeterminers Empty Empty {} {}


main = log "Done"

