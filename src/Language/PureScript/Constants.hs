-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Constants
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Various constants which refer to things in the Prelude
--
-----------------------------------------------------------------------------

module Language.PureScript.Constants where

-- Prelude Operators

($) :: String
($) = "$"

(#) :: String
(#) = "#"

(!!) :: String
(!!) = "!!"

(++) :: String
(++) = "++"

(>>=) :: String
(>>=) = ">>="

(+) :: String
(+) = "+"

(-) :: String
(-) = "-"

(*) :: String
(*) = "*"

(/) :: String
(/) = "/"

(%) :: String
(%) = "%"

(<) :: String
(<) = "<"

(>) :: String
(>) = ">"

(<=) :: String
(<=) = "<="

(>=) :: String
(>=) = ">="

(==) :: String
(==) = "=="

(/=) :: String
(/=) = "/="

(&) :: String
(&) = "&"

bar :: String
bar = "|"

(^) :: String
(^) = "^"

(&&) :: String
(&&) = "&&"

(||) :: String
(||) = "||"

-- Prelude Operator Functions

negate :: String
negate = "negate"

shl :: String
shl = "shl"

shr :: String
shr = "shr"

zshr :: String
zshr = "zshr"

complement :: String
complement = "complement"

not :: String
not = "not"

-- Prelude Values

return :: String
return = "return"

returnEscaped :: String
returnEscaped = "$return"

untilE :: String
untilE = "untilE"

whileE :: String
whileE = "whileE"

runST :: String
runST = "runST"

runSTArray :: String
runSTArray = "runSTArray"

stRefValue :: String
stRefValue = "value"

newSTRef :: String
newSTRef = "newSTRef"

readSTRef :: String
readSTRef = "readSTRef"

writeSTRef :: String
writeSTRef = "writeSTRef"

modifySTRef :: String
modifySTRef = "modifySTRef"

peekSTArray :: String
peekSTArray = "peekSTArray"

pokeSTArray :: String
pokeSTArray = "pokeSTArray"

-- Type Class Dictionary Names

monadEffDictionary :: String
monadEffDictionary = "monadEff"

numNumber :: String
numNumber = "numNumber"

ordNumber :: String
ordNumber = "ordNumber"

eqNumber :: String
eqNumber = "eqNumber"

eqString :: String
eqString = "eqString"

eqBoolean :: String
eqBoolean = "eqBoolean"

bitsNumber :: String
bitsNumber = "bitsNumber"

boolLikeBoolean :: String
boolLikeBoolean = "boolLikeBoolean"

-- Main module

main :: String
main = "main"

-- Code Generation

_ps :: String
_ps = "_ps"

__superclasses :: String
__superclasses = "__superclasses"

-- Modules

prim :: String
prim = "Prim"

prelude :: String
prelude = "Prelude"

eff :: String
eff = "Control_Monad_Eff"

st :: String
st = "Control_Monad_ST"
