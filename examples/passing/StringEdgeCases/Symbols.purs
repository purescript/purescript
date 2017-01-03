-- This is similar to StringEscapes except we are performing the same tests
-- with Symbols (at the type level).

module Symbols where

import Prelude
import Control.Monad.Eff.Console (log)
import Type.Data.Symbol (SProxy(..), class AppendSymbol, appendSymbol, reflectSymbol)
import Test.Assert (assert')

highS :: SProxy "\xd834"
highS = SProxy

lowS :: SProxy "\xdf06"
lowS = SProxy

loneSurrogates :: Boolean
loneSurrogates = reflectSymbol (appendSymbol highS lowS) == "\x1d306"

outOfOrderSurrogates :: Boolean
outOfOrderSurrogates = reflectSymbol (appendSymbol lowS highS) == "\xdf06\xd834"

notReplacing :: Boolean
notReplacing = reflectSymbol lowS /= "\xfffd"

main = do
  assert' "lone surrogates may be combined into a surrogate pair" loneSurrogates
  assert' "lone surrogates may be combined out of order to remain lone surrogates" outOfOrderSurrogates
  assert' "lone surrogates are not replaced with the Unicode replacement character U+FFFD" notReplacing
  log "Done"
