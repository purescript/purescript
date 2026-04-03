module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assert)
import Data.Newtype (class Newtype, unwrap)

newtype Username = Username String
  derive (Newtype Username _)
  derive newtype (Eq, Show)

newtype TodoId = TodoId Int
  derive (Newtype)
  derive newtype (Eq, Show)

main = do
  assert $ (unwrap (Username "alice") :: String) == "alice"
  assert $ Username "bob" == Username "bob"
  assert $ show (Username "carol") == "\"carol\""
  assert $ (unwrap (TodoId 1) :: Int) == 1
  assert $ TodoId 2 == TodoId 2
  assert $ show (TodoId 3) == "3"
  log "Done"
