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

-- Operators

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

(&&) :: String
(&&) = "&&"

(||) :: String
(||) = "||"

bind :: String
bind = "bind"

unsafeIndex :: String
unsafeIndex = "unsafeIndex"

(.|.) :: String
(.|.) = ".|."

(.&.) :: String
(.&.) = ".&."

(.^.) :: String
(.^.) = ".^."

(<<<) :: String
(<<<) = "<<<"

compose :: String
compose = "compose"

-- Functions

negate :: String
negate = "negate"

not :: String
not = "not"

conj :: String
conj = "conj"

disj :: String
disj = "disj"

mod :: String
mod = "mod"

shl :: String
shl = "shl"

shr :: String
shr = "shr"

zshr :: String
zshr = "zshr"

complement :: String
complement = "complement"

-- Prelude Values

zero :: String
zero = "zero"

one :: String
one = "one"

bottom :: String
bottom = "bottom"

top :: String
top = "top"

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

mkFn :: String
mkFn = "mkFn"

runFn :: String
runFn = "runFn"

unit :: String
unit = "unit"

-- Prim values

undefined :: String
undefined = "undefined"

-- Type Class Dictionary Names

monadEffDictionary :: String
monadEffDictionary = "monadEff"

applicativeEffDictionary :: String
applicativeEffDictionary = "applicativeEff"

bindEffDictionary :: String
bindEffDictionary = "bindEff"

semiringNumber :: String
semiringNumber = "semiringNumber"

semiringInt :: String
semiringInt = "semiringInt"

ringNumber :: String
ringNumber = "ringNumber"

ringInt :: String
ringInt = "ringInt"

moduloSemiringNumber :: String
moduloSemiringNumber = "moduloSemiringNumber"

moduloSemiringInt :: String
moduloSemiringInt = "moduloSemiringInt"

ordNumber :: String
ordNumber = "ordNumber"

ordInt :: String
ordInt = "ordInt"

eqNumber :: String
eqNumber = "eqNumber"

eqInt :: String
eqInt = "eqInt"

eqString :: String
eqString = "eqString"

eqBoolean :: String
eqBoolean = "eqBoolean"

boundedBoolean :: String
boundedBoolean = "boundedBoolean"

booleanAlgebraBoolean :: String
booleanAlgebraBoolean = "booleanAlgebraBoolean"

semigroupString :: String
semigroupString = "semigroupString"

semigroupoidFn :: String
semigroupoidFn = "semigroupoidFn"

-- Generic Deriving

generic :: String
generic = "Generic"

toSpine :: String
toSpine = "toSpine"

fromSpine :: String
fromSpine = "fromSpine"

toSignature :: String
toSignature = "toSignature"

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

dataArrayUnsafe :: String
dataArrayUnsafe = "Data_Array_Unsafe"

eff :: String
eff = "Control_Monad_Eff"

st :: String
st = "Control_Monad_ST"

dataFunction :: String
dataFunction = "Data_Function"

dataIntBits :: String
dataIntBits = "Data_Int_Bits"
