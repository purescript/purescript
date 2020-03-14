-- @shouldFailWith KindsDoNotUnify
module Main where

data TProxy (t :: Type) = TProxy
data SProxy (s :: Symbol) = SProxy

put :: forall proxy a. proxy a -> TProxy a
put _ = TProxy

--wrong :: TProxy "apple"
wrong = put (SProxy :: SProxy "apple")
