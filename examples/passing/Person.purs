module Person where

  import Prelude ((++))

  data Person = MkPerson { name :: String, age :: Number }

  foreign import itoa :: Number -> String
	  
  showPerson :: Person -> String
  showPerson = \p -> case p of
    MkPerson o -> o.name ++ ", aged " ++ itoa(o.age)
    
module Main where

main = Trace.trace "Done"
