-- @shouldFailWith NoInstanceFound
module Main where
	
import Prelude

newtype Foo r = Foo { | r }

derive instance eqFoo :: Eq (Foo r)
derive instance ordFoo :: Ord (Foo r)
