-- | Various constants which refer to things in the Prelude
module Language.PureScript.Constants where

import Prelude.Compat

import Language.PureScript.Names

-- Operators

($) :: String
($) = "$"

apply :: String
apply = "apply"

(#) :: String
(#) = "#"

applyFlipped :: String
applyFlipped = "applyFlipped"

(<>) :: String
(<>) = "<>"

(++) :: String
(++) = "++"

append :: String
append = "append"

(>>=) :: String
(>>=) = ">>="

bind :: String
bind = "bind"

(+) :: String
(+) = "+"

add :: String
add = "add"

(-) :: String
(-) = "-"

sub :: String
sub = "sub"

(*) :: String
(*) = "*"

mul :: String
mul = "mul"

(/) :: String
(/) = "/"

div :: String
div = "div"

(%) :: String
(%) = "%"

mod :: String
mod = "mod"

(<) :: String
(<) = "<"

lessThan :: String
lessThan = "lessThan"

(>) :: String
(>) = ">"

greaterThan :: String
greaterThan = "greaterThan"

(<=) :: String
(<=) = "<="

lessThanOrEq :: String
lessThanOrEq = "lessThanOrEq"

(>=) :: String
(>=) = ">="

greaterThanOrEq :: String
greaterThanOrEq = "greaterThanOrEq"

(==) :: String
(==) = "=="

eq :: String
eq = "eq"

(/=) :: String
(/=) = "/="

notEq :: String
notEq = "notEq"

compare :: String
compare = "compare"

(&&) :: String
(&&) = "&&"

conj :: String
conj = "conj"

(||) :: String
(||) = "||"

disj :: String
disj = "disj"

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

(>>>) :: String
(>>>) = ">>>"

composeFlipped :: String
composeFlipped = "composeFlipped"

-- Functions

negate :: String
negate = "negate"

not :: String
not = "not"

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

euclideanRingNumber :: String
euclideanRingNumber = "euclideanRingNumber"

euclideanRingInt :: String
euclideanRingInt = "euclideanRingInt"

ordBoolean :: String
ordBoolean = "ordBoolean"

ordNumber :: String
ordNumber = "ordNumber"

ordInt :: String
ordInt = "ordInt"

ordString :: String
ordString = "ordString"

ordChar :: String
ordChar = "ordChar"

eqNumber :: String
eqNumber = "eqNumber"

eqInt :: String
eqInt = "eqInt"

eqString :: String
eqString = "eqString"

eqChar :: String
eqChar = "eqChar"

eqBoolean :: String
eqBoolean = "eqBoolean"

boundedBoolean :: String
boundedBoolean = "boundedBoolean"

booleanAlgebraBoolean :: String
booleanAlgebraBoolean = "booleanAlgebraBoolean"

heytingAlgebraBoolean :: String
heytingAlgebraBoolean = "heytingAlgebraBoolean"

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

-- Prim

partial :: String
partial = "Partial"

pattern Partial :: Qualified (ProperName 'ClassName)
pattern Partial = Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Partial")

pattern Fail :: Qualified (ProperName 'ClassName)
pattern Fail = Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Fail")

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

dataArray :: String
dataArray = "Data_Array"

eff :: String
eff = "Control_Monad_Eff"

st :: String
st = "Control_Monad_ST"

controlApplicative :: String
controlApplicative = "Control_Applicative"

controlSemigroupoid :: String
controlSemigroupoid = "Control_Semigroupoid"

controlBind :: String
controlBind = "Control_Bind"

dataBounded :: String
dataBounded = "Data_Bounded"

dataSemigroup :: String
dataSemigroup = "Data_Semigroup"

dataHeytingAlgebra :: String
dataHeytingAlgebra = "Data_HeytingAlgebra"

dataEq :: String
dataEq = "Data_Eq"

dataOrd :: String
dataOrd = "Data_Ord"

dataSemiring :: String
dataSemiring = "Data_Semiring"

dataRing :: String
dataRing = "Data_Ring"

dataEuclideanRing :: String
dataEuclideanRing = "Data_EuclideanRing"

dataFunction :: String
dataFunction = "Data_Function"

dataFunctionUncurried :: String
dataFunctionUncurried = "Data_Function_Uncurried"

dataIntBits :: String
dataIntBits = "Data_Int_Bits"
