module RecordLabels where

import Prelude
import Data.Generic (class Generic, gShow)
import Control.Monad.Eff.Console (log)
import Test.Assert (assert')

newtype AstralKeys = AstralKeys { "💡" :: Int, "💢" :: Int }
newtype LoneSurrogateKeys = LoneSurrogateKeys { "\xdf06" :: Int, "\xd834" :: Int }

derive instance genericAstralKeys :: Generic AstralKeys
derive instance genericLoneSurrogateKeys :: Generic LoneSurrogateKeys

loneSurrogateKeys =
  gShow (LoneSurrogateKeys { "\xdf06": 0, "\xd834": 1 }) ==
    """LoneSurrogateKeys { "\xdf06": 0, "\xd834": 1 }"""

astralKeys =
  gShow (AstralKeys { "💡": 0, "💢": 1 }) ==
    """AstralKeys { "💡": 0, "💢": 1 }"""

main = do
  assert' "lone surrogate keys" loneSurrogateKeys
  assert' "astral keys" astralKeys
  log "Done"
