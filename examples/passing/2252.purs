module Main where
	
import Control.Monad.Eff.Console (log)

data T a = T
	 
ti :: T Int
ti = T

t :: forall a. T a
t = T

xs = [ti, t, t]

main = log "Done"
