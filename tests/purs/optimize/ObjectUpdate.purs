module Main where

staticUpdate1 :: { alpha :: Int, bravo :: String } -> { alpha :: Int, bravo :: String }
staticUpdate1 x = x { bravo = "replaced" }

staticUpdate2 :: { alpha :: Int, bravo :: String } -> { alpha :: Int, bravo :: Boolean }
staticUpdate2 x = x { bravo = true }

dynamicUpdate1 :: forall r. { alpha :: Int, bravo :: String | r } -> { alpha :: Int, bravo :: Boolean | r }
dynamicUpdate1 x = x { bravo = true }
