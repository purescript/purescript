module Main where
	
import Prelude
import Control.Monad.Eff.Console (log)

type L = {}
data X = X L
derive instance eqX :: Eq X

type M = {}
data Y = Y {foo :: M}
derive instance eqY :: Eq Y

type N = {}
data Z = Z N
derive instance eqZ :: Eq Z

type Foo = String

type Bar = { foo :: Foo }

type Baz = { baz :: Bar }

newtype T = T Baz

derive instance eqT :: Eq T
derive instance ordT :: Ord T

main = log "Done"
