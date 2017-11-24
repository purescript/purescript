module Records where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assert')

newtype AstralKeys = AstralKeys { "💡" :: Int, "💢" :: Int }
newtype LoneSurrogateKeys = LoneSurrogateKeys { "\xdf06" :: Int, "\xd834" :: Int }

testLoneSurrogateKeys =
  let
    expected = 5
    actual = (_."\xd801" <<< helper) { "\xd800": 5 }
  in
    assert' ("lone surrogate keys: " <> show actual) (expected == actual)

  where
  helper :: { "\xd800" :: Int } -> { "\xd801" :: Int }
  helper o =
    case o."\xd800" of
      x -> { "\xd801": x }

testAstralKeys =
  let
    expected = 5
    actual = (_."💢" <<< helper) { "💡": 5 }
  in
    assert' ("astral keys: " <> show actual) (expected == actual)

  where
  helper :: { "💡" :: Int } -> { "💢" :: Int }
  helper o =
    case o."💡" of
      x -> { "💢": x }

main = do
  testLoneSurrogateKeys
  testAstralKeys
  log "Done"
