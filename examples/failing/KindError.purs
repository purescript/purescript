module Main where

  data KindError f a = One f | Two (f a)
