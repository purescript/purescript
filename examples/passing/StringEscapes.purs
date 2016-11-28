module Main where

import Prelude ((==), (/=), (<>), bind)
import Test.Assert (assert, assert')
import Control.Monad.Eff.Console (log)

singleCharacter = "\0\b\t\n\v\f\r\"\\" == "\x0\x8\x9\xA\xB\xC\xD\x22\x5C"
hex = "\x1D306\x2603\x3C6\xE0\x0" == "𝌆☃φà\0"
decimal  = "\119558\9731\966\224\0" == "𝌆☃φà\0"
surrogatePair = "\xD834\xDF06" == "\x1D306"
highSurrogate = "\xD834"
lowSurrogate = "\xDF06"
loneSurrogates = (highSurrogate <> lowSurrogate) == "\x1D306"
outOfOrderSurrogates = (lowSurrogate <> highSurrogate) == "\xDF06\xD834"
replacement = "\xFFFD"
notReplacing = replacement /= highSurrogate

main = do
  assert' "single-character escape sequences" singleCharacter
  assert' "hex escape sequences" hex
  assert' "decimal escape sequences" decimal
  assert' "astral code points are represented as a UTF-16 surrogate pair" surrogatePair
  assert' "lone surrogates may be combined into a surrogate pair" loneSurrogates
  assert' "lone surrogates may be combined out of order to remain lone surrogates" outOfOrderSurrogates
  assert' "lone surrogates are not replaced with the Unicode replacement character U+FFFD" notReplacing
  log "Done"
