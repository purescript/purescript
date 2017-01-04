module Records where

import Prelude
import Data.Generic (class Generic, toSpine, GenericSpine(..))
import Control.Monad.Eff.Console (log)
import Test.Assert (assert')

newtype AstralKeys = AstralKeys { "💡" :: Int, "💢" :: Int }
newtype LoneSurrogateKeys = LoneSurrogateKeys { "\xdf06" :: Int, "\xd834" :: Int }

derive instance genericAstralKeys :: Generic AstralKeys
derive instance genericLoneSurrogateKeys :: Generic LoneSurrogateKeys

spineOf :: forall a. Generic a => a -> Unit -> GenericSpine
spineOf x _ = toSpine x

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

testGenericLoneSurrogateKeys = do
  let expected = SProd "Records.LoneSurrogateKeys"
                  [ \_ -> SRecord [ {recLabel: "\xd834", recValue: spineOf 1}
                                  , {recLabel: "\xdf06", recValue: spineOf 0}
                                  ]
                  ]
      actual = toSpine (LoneSurrogateKeys { "\xdf06": 0, "\xd834": 1 })
  assert' ("generic lone surrogate keys: " <> show actual) (expected == actual)

testGenericAstralKeys = do
  let expected = SProd "Records.AstralKeys"
                  [ \_ -> SRecord [ {recLabel: "💡", recValue: spineOf 0}
                                  , {recLabel: "💢", recValue: spineOf 1}
                                  ]
                  ]
      actual = toSpine (AstralKeys { "💡": 0, "💢": 1 })
  assert' ("generic astral keys: " <> show actual) (expected == actual)

main = do
  testLoneSurrogateKeys
  testAstralKeys
  testGenericLoneSurrogateKeys
  testGenericAstralKeys
  log "Done"
