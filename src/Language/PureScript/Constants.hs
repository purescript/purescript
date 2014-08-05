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

(<>) :: String
(<>) = "<>"

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

unsafeIndex :: String
unsafeIndex = "unsafeIndex"

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

pure' :: String
pure' = "pure"

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

mkFn :: String
mkFn = "mkFn"

runFn :: String
runFn = "runFn"

-- Type Class Dictionary Names

monadEffDictionary :: String
monadEffDictionary = "monadEff"

applicativeEffDictionary :: String
applicativeEffDictionary = "applicativeEff"

bindEffDictionary :: String
bindEffDictionary = "bindEff"

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

semigroupString :: String
semigroupString = "semigroupString"

semigroupoidArr :: String
semigroupoidArr = "semigroupoidArr"

-- Main module

main :: String
main = "main"

-- Code Generation

__superclass_ :: String
__superclass_ = "__superclass_"

__unused :: String
__unused = "__unused"

-- Modules

prim :: String
prim = "Prim"

prelude :: String
prelude = "Prelude"

preludeUnsafe :: String
preludeUnsafe = "Prelude_Unsafe"

eff :: String
eff = "Control_Monad_Eff"

st :: String
st = "Control_Monad_ST"

dataFunction :: String
dataFunction = "Data_Function"
