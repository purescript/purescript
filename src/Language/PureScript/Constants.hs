-- | Various constants which refer to things in the Prelude
module Language.PureScript.Constants where

import Prelude.Compat
import Data.Text (Text)
import Language.PureScript.Names

-- Operators

($) :: Text
($) = "$"

apply :: Text
apply = "apply"

(#) :: Text
(#) = "#"

applyFlipped :: Text
applyFlipped = "applyFlipped"

(<>) :: Text
(<>) = "<>"

(++) :: Text
(++) = "++"

append :: Text
append = "append"

(>>=) :: Text
(>>=) = ">>="

bind :: Text
bind = "bind"

(+) :: Text
(+) = "+"

add :: Text
add = "add"

(-) :: Text
(-) = "-"

sub :: Text
sub = "sub"

(*) :: Text
(*) = "*"

mul :: Text
mul = "mul"

(/) :: Text
(/) = "/"

div :: Text
div = "div"

(%) :: Text
(%) = "%"

mod :: Text
mod = "mod"

(<) :: Text
(<) = "<"

lessThan :: Text
lessThan = "lessThan"

(>) :: Text
(>) = ">"

greaterThan :: Text
greaterThan = "greaterThan"

(<=) :: Text
(<=) = "<="

lessThanOrEq :: Text
lessThanOrEq = "lessThanOrEq"

(>=) :: Text
(>=) = ">="

greaterThanOrEq :: Text
greaterThanOrEq = "greaterThanOrEq"

(==) :: Text
(==) = "=="

eq :: Text
eq = "eq"

(/=) :: Text
(/=) = "/="

notEq :: Text
notEq = "notEq"

compare :: Text
compare = "compare"

(&&) :: Text
(&&) = "&&"

conj :: Text
conj = "conj"

(||) :: Text
(||) = "||"

disj :: Text
disj = "disj"

unsafeIndex :: Text
unsafeIndex = "unsafeIndex"

or :: Text
or = "or"

and :: Text
and = "and"

xor :: Text
xor = "xor"

(<<<) :: Text
(<<<) = "<<<"

compose :: Text
compose = "compose"

(>>>) :: Text
(>>>) = ">>>"

composeFlipped :: Text
composeFlipped = "composeFlipped"

map :: Text
map = "map"

-- Functions

negate :: Text
negate = "negate"

not :: Text
not = "not"

shl :: Text
shl = "shl"

shr :: Text
shr = "shr"

zshr :: Text
zshr = "zshr"

complement :: Text
complement = "complement"

-- Prelude Values

zero :: Text
zero = "zero"

one :: Text
one = "one"

bottom :: Text
bottom = "bottom"

top :: Text
top = "top"

return :: Text
return = "return"

pure' :: Text
pure' = "pure"

returnEscaped :: Text
returnEscaped = "$return"

untilE :: Text
untilE = "untilE"

whileE :: Text
whileE = "whileE"

runST :: Text
runST = "runST"

stRefValue :: Text
stRefValue = "value"

newSTRef :: Text
newSTRef = "newSTRef"

readSTRef :: Text
readSTRef = "readSTRef"

writeSTRef :: Text
writeSTRef = "writeSTRef"

modifySTRef :: Text
modifySTRef = "modifySTRef"

mkFn :: Text
mkFn = "mkFn"

runFn :: Text
runFn = "runFn"

unit :: Text
unit = "unit"

-- Prim values

undefined :: Text
undefined = "undefined"

-- Type Class Dictionary Names

monadEffDictionary :: Text
monadEffDictionary = "monadEff"

applicativeEffDictionary :: Text
applicativeEffDictionary = "applicativeEff"

bindEffDictionary :: Text
bindEffDictionary = "bindEff"

semiringNumber :: Text
semiringNumber = "semiringNumber"

semiringInt :: Text
semiringInt = "semiringInt"

ringNumber :: Text
ringNumber = "ringNumber"

ringInt :: Text
ringInt = "ringInt"

moduloSemiringNumber :: Text
moduloSemiringNumber = "moduloSemiringNumber"

moduloSemiringInt :: Text
moduloSemiringInt = "moduloSemiringInt"

euclideanRingNumber :: Text
euclideanRingNumber = "euclideanRingNumber"

euclideanRingInt :: Text
euclideanRingInt = "euclideanRingInt"

ordBoolean :: Text
ordBoolean = "ordBoolean"

ordNumber :: Text
ordNumber = "ordNumber"

ordInt :: Text
ordInt = "ordInt"

ordString :: Text
ordString = "ordString"

ordChar :: Text
ordChar = "ordChar"

eqNumber :: Text
eqNumber = "eqNumber"

eqInt :: Text
eqInt = "eqInt"

eqString :: Text
eqString = "eqString"

eqChar :: Text
eqChar = "eqChar"

eqBoolean :: Text
eqBoolean = "eqBoolean"

boundedBoolean :: Text
boundedBoolean = "boundedBoolean"

booleanAlgebraBoolean :: Text
booleanAlgebraBoolean = "booleanAlgebraBoolean"

heytingAlgebraBoolean :: Text
heytingAlgebraBoolean = "heytingAlgebraBoolean"

semigroupString :: Text
semigroupString = "semigroupString"

semigroupoidFn :: Text
semigroupoidFn = "semigroupoidFn"

-- Generic Deriving

generic :: Text
generic = "Generic"

toSpine :: Text
toSpine = "toSpine"

fromSpine :: Text
fromSpine = "fromSpine"

toSignature :: Text
toSignature = "toSignature"

-- Data.Symbol

pattern DataSymbol :: ModuleName
pattern DataSymbol = ModuleName [ProperName "Data", ProperName "Symbol"]

pattern IsSymbol :: Qualified (ProperName 'ClassName)
pattern IsSymbol = Qualified (Just DataSymbol) (ProperName "IsSymbol")

-- Type.Data.Symbol

pattern TypeDataSymbol :: ModuleName
pattern TypeDataSymbol = ModuleName [ProperName "Type", ProperName "Data", ProperName "Symbol"]

pattern CompareSymbol :: Qualified (ProperName 'ClassName)
pattern CompareSymbol = Qualified (Just TypeDataSymbol) (ProperName "CompareSymbol")

pattern AppendSymbol :: Qualified (ProperName 'ClassName)
pattern AppendSymbol = Qualified (Just TypeDataSymbol) (ProperName "AppendSymbol")

-- Type.Data.Ordering

typeDataOrdering :: ModuleName
typeDataOrdering = ModuleName [ProperName "Type", ProperName "Data", ProperName "Ordering"]

orderingLT :: Qualified (ProperName 'TypeName)
orderingLT = Qualified (Just typeDataOrdering) (ProperName "LT")

orderingEQ :: Qualified (ProperName 'TypeName)
orderingEQ = Qualified (Just typeDataOrdering) (ProperName "EQ")

orderingGT :: Qualified (ProperName 'TypeName)
orderingGT = Qualified (Just typeDataOrdering) (ProperName "GT")

-- Main module

main :: Text
main = "main"

-- Prim

partial :: Text
partial = "Partial"

pattern Prim :: ModuleName
pattern Prim = ModuleName [ProperName "Prim"]

pattern Partial :: Qualified (ProperName 'ClassName)
pattern Partial = Qualified (Just Prim) (ProperName "Partial")

pattern Fail :: Qualified (ProperName 'ClassName)
pattern Fail = Qualified (Just Prim) (ProperName "Fail")

typ :: Text
typ = "Type"

effect :: Text
effect = "Effect"

symbol :: Text
symbol = "Symbol"

-- Code Generation

__superclass_ :: Text
__superclass_ = "__superclass_"

__unused :: Text
__unused = "__unused"

-- Modules

prim :: Text
prim = "Prim"

prelude :: Text
prelude = "Prelude"

dataArray :: Text
dataArray = "Data_Array"

eff :: Text
eff = "Control_Monad_Eff"

st :: Text
st = "Control_Monad_ST"

controlApplicative :: Text
controlApplicative = "Control_Applicative"

controlSemigroupoid :: Text
controlSemigroupoid = "Control_Semigroupoid"

controlBind :: Text
controlBind = "Control_Bind"

dataBounded :: Text
dataBounded = "Data_Bounded"

dataSemigroup :: Text
dataSemigroup = "Data_Semigroup"

dataHeytingAlgebra :: Text
dataHeytingAlgebra = "Data_HeytingAlgebra"

dataEq :: Text
dataEq = "Data_Eq"

dataOrd :: Text
dataOrd = "Data_Ord"

dataSemiring :: Text
dataSemiring = "Data_Semiring"

dataRing :: Text
dataRing = "Data_Ring"

dataEuclideanRing :: Text
dataEuclideanRing = "Data_EuclideanRing"

dataFunction :: Text
dataFunction = "Data_Function"

dataFunctionUncurried :: Text
dataFunctionUncurried = "Data_Function_Uncurried"

dataIntBits :: Text
dataIntBits = "Data_Int_Bits"
