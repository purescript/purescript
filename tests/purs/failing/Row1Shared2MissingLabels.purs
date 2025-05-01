-- @shouldFailWith TypesDoNotUnify
module Row1Shared2MissingLabels where

data Tuple a b = Tuple a b

doStuff
  :: Tuple { shared :: Int, qOnly :: String }
           { shared :: Int, zOnly :: Char }
  -> Int
doStuff (Tuple q z) = doStuff (Tuple z q)
