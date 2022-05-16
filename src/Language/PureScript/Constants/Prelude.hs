-- | Various constants which refer to things in the Prelude
module Language.PureScript.Constants.Prelude where

import Prelude.Compat hiding (compare, map)

import Data.String (IsString)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Names

-- Operators

apply :: forall a. (IsString a) => a
apply = "apply"

applyFlipped :: forall a. (IsString a) => a
applyFlipped = "applyFlipped"

append :: forall a. (IsString a) => a
append = "append"

bind :: forall a. (IsString a) => a
bind = "bind"

discard :: forall a. (IsString a) => a
discard = "discard"

pattern Discard :: Qualified (ProperName 'ClassName)
pattern Discard = Qualified (Just ControlBind) (ProperName "Discard")

add :: forall a. (IsString a) => a
add = "add"

sub :: forall a. (IsString a) => a
sub = "sub"

mul :: forall a. (IsString a) => a
mul = "mul"

div :: forall a. (IsString a) => a
div = "div"

lessThan :: forall a. (IsString a) => a
lessThan = "lessThan"

greaterThan :: forall a. (IsString a) => a
greaterThan = "greaterThan"

lessThanOrEq :: forall a. (IsString a) => a
lessThanOrEq = "lessThanOrEq"

greaterThanOrEq :: forall a. (IsString a) => a
greaterThanOrEq = "greaterThanOrEq"

eq :: forall a. (IsString a) => a
eq = "eq"

eq1 :: forall a. (IsString a) => a
eq1 = "eq1"

notEq :: forall a. (IsString a) => a
notEq = "notEq"

compare :: forall a. (IsString a) => a
compare = "compare"

compare1 :: forall a. (IsString a) => a
compare1 = "compare1"

conj :: forall a. (IsString a) => a
conj = "conj"

disj :: forall a. (IsString a) => a
disj = "disj"

unsafeIndex :: forall a. (IsString a) => a
unsafeIndex = "unsafeIndex"

or :: forall a. (IsString a) => a
or = "or"

and :: forall a. (IsString a) => a
and = "and"

xor :: forall a. (IsString a) => a
xor = "xor"

compose :: forall a. (IsString a) => a
compose = "compose"

composeFlipped :: forall a. (IsString a) => a
composeFlipped = "composeFlipped"

map :: forall a. (IsString a) => a
map = "map"

-- Functions

negate :: forall a. (IsString a) => a
negate = "negate"

not :: forall a. (IsString a) => a
not = "not"

shl :: forall a. (IsString a) => a
shl = "shl"

shr :: forall a. (IsString a) => a
shr = "shr"

zshr :: forall a. (IsString a) => a
zshr = "zshr"

complement :: forall a. (IsString a) => a
complement = "complement"

-- Prelude Values

zero :: forall a. (IsString a) => a
zero = "zero"

one :: forall a. (IsString a) => a
one = "one"

bottom :: forall a. (IsString a) => a
bottom = "bottom"

top :: forall a. (IsString a) => a
top = "top"

pure' :: forall a. (IsString a) => a
pure' = "pure"

-- Core lib values

runST :: forall a. (IsString a) => a
runST = "run"

stRefValue :: forall a. (IsString a) => a
stRefValue = "value"

newSTRef :: forall a. (IsString a) => a
newSTRef = "new"

readSTRef :: forall a. (IsString a) => a
readSTRef = "read"

writeSTRef :: forall a. (IsString a) => a
writeSTRef = "write"

modifySTRef :: forall a. (IsString a) => a
modifySTRef = "modify"

mkFn :: forall a. (IsString a) => a
mkFn = "mkFn"

runFn :: forall a. (IsString a) => a
runFn = "runFn"

mkEffFn :: forall a. (IsString a) => a
mkEffFn = "mkEffFn"

runEffFn :: forall a. (IsString a) => a
runEffFn = "runEffFn"

mkEffectFn :: forall a. (IsString a) => a
mkEffectFn = "mkEffectFn"

runEffectFn :: forall a. (IsString a) => a
runEffectFn = "runEffectFn"

-- Type Class Dictionary Names

data EffectDictionaries = EffectDictionaries
  { edApplicativeDict :: PSString
  , edBindDict :: PSString
  , edMonadDict :: PSString
  , edWhile :: PSString
  , edUntil :: PSString
  }

effDictionaries :: EffectDictionaries
effDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEff"
  , edBindDict = "bindEff"
  , edMonadDict = "monadEff"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

effectDictionaries :: EffectDictionaries
effectDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeEffect"
  , edBindDict = "bindEffect"
  , edMonadDict = "monadEffect"
  , edWhile = "whileE"
  , edUntil = "untilE"
  }

stDictionaries :: EffectDictionaries
stDictionaries = EffectDictionaries
  { edApplicativeDict = "applicativeST"
  , edBindDict = "bindST"
  , edMonadDict = "monadST"
  , edWhile = "while"
  , edUntil = "until"
  }

discardUnitDictionary :: forall a. (IsString a) => a
discardUnitDictionary = "discardUnit"

semiringNumber :: forall a. (IsString a) => a
semiringNumber = "semiringNumber"

semiringInt :: forall a. (IsString a) => a
semiringInt = "semiringInt"

ringNumber :: forall a. (IsString a) => a
ringNumber = "ringNumber"

ringInt :: forall a. (IsString a) => a
ringInt = "ringInt"

euclideanRingNumber :: forall a. (IsString a) => a
euclideanRingNumber = "euclideanRingNumber"

ordBoolean :: forall a. (IsString a) => a
ordBoolean = "ordBoolean"

ordNumber :: forall a. (IsString a) => a
ordNumber = "ordNumber"

ordInt :: forall a. (IsString a) => a
ordInt = "ordInt"

ordString :: forall a. (IsString a) => a
ordString = "ordString"

ordChar :: forall a. (IsString a) => a
ordChar = "ordChar"

eqNumber :: forall a. (IsString a) => a
eqNumber = "eqNumber"

eqInt :: forall a. (IsString a) => a
eqInt = "eqInt"

eqString :: forall a. (IsString a) => a
eqString = "eqString"

eqChar :: forall a. (IsString a) => a
eqChar = "eqChar"

eqBoolean :: forall a. (IsString a) => a
eqBoolean = "eqBoolean"

boundedBoolean :: forall a. (IsString a) => a
boundedBoolean = "boundedBoolean"

heytingAlgebraBoolean :: forall a. (IsString a) => a
heytingAlgebraBoolean = "heytingAlgebraBoolean"

semigroupString :: forall a. (IsString a) => a
semigroupString = "semigroupString"

semigroupoidFn :: forall a. (IsString a) => a
semigroupoidFn = "semigroupoidFn"

-- Data.Symbol

pattern DataSymbol :: ModuleName
pattern DataSymbol = ModuleName "Data.Symbol"

pattern IsSymbol :: Qualified (ProperName 'ClassName)
pattern IsSymbol = Qualified (Just DataSymbol) (ProperName "IsSymbol")

pattern DataReflectable :: ModuleName
pattern DataReflectable = ModuleName "Data.Reflectable"

pattern Reflectable :: Qualified (ProperName 'ClassName)
pattern Reflectable = Qualified (Just DataReflectable) (ProperName "Reflectable")

pattern DataOrdering :: ModuleName
pattern DataOrdering = ModuleName "Data.Ordering"

pattern DataFunctionUncurried :: ModuleName
pattern DataFunctionUncurried = ModuleName "Data.Function.Uncurried"

pattern PartialUnsafe :: ModuleName
pattern PartialUnsafe = ModuleName "Partial.Unsafe"

pattern Ordering :: Qualified (ProperName 'TypeName)
pattern Ordering = Qualified (Just DataOrdering) (ProperName "Ordering")

pattern LT :: Qualified (ProperName 'ConstructorName)
pattern LT = Qualified (Just DataOrdering) (ProperName "LT")

pattern EQ :: Qualified (ProperName 'ConstructorName)
pattern EQ = Qualified (Just DataOrdering) (ProperName "EQ")

pattern GT :: Qualified (ProperName 'ConstructorName)
pattern GT = Qualified (Just DataOrdering) (ProperName "GT")

pattern DataArray :: ModuleName
pattern DataArray = ModuleName "Data.Array"

pattern Eff :: ModuleName
pattern Eff = ModuleName "Control.Monad.Eff"

pattern Effect :: ModuleName
pattern Effect = ModuleName "Effect"

pattern ST :: ModuleName
pattern ST = ModuleName "Control.Monad.ST.Internal"

pattern ControlApplicative :: ModuleName
pattern ControlApplicative = ModuleName "Control.Applicative"

pattern ControlSemigroupoid :: ModuleName
pattern ControlSemigroupoid = ModuleName "Control.Semigroupoid"

pattern ControlBind :: ModuleName
pattern ControlBind = ModuleName "Control.Bind"

pattern ControlMonadEffUncurried :: ModuleName
pattern ControlMonadEffUncurried = ModuleName "Control.Monad.Eff.Uncurried"

pattern EffectUncurried :: ModuleName
pattern EffectUncurried = ModuleName "Effect.Uncurried"

pattern DataBounded :: ModuleName
pattern DataBounded = ModuleName "Data.Bounded"

pattern DataSemigroup :: ModuleName
pattern DataSemigroup = ModuleName "Data.Semigroup"

pattern DataHeytingAlgebra :: ModuleName
pattern DataHeytingAlgebra = ModuleName "Data.HeytingAlgebra"

pattern DataEq :: ModuleName
pattern DataEq = ModuleName "Data.Eq"

pattern Eq :: Qualified (ProperName 'ClassName)
pattern Eq = Qualified (Just DataEq) (ProperName "Eq")

pattern Eq1 :: Qualified (ProperName 'ClassName)
pattern Eq1 = Qualified (Just DataEq) (ProperName "Eq1")

identEq :: Qualified Ident
identEq = Qualified (Just DataEq) (Ident eq)

identEq1 :: Qualified Ident
identEq1 = Qualified (Just DataEq) (Ident eq1)

pattern DataOrd :: ModuleName
pattern DataOrd = ModuleName "Data.Ord"

pattern Ord :: Qualified (ProperName 'ClassName)
pattern Ord = Qualified (Just DataOrd) (ProperName "Ord")

pattern Ord1 :: Qualified (ProperName 'ClassName)
pattern Ord1 = Qualified (Just DataOrd) (ProperName "Ord1")

identCompare :: Qualified Ident
identCompare = Qualified (Just DataOrd) (Ident compare)

identCompare1 :: Qualified Ident
identCompare1 = Qualified (Just DataOrd) (Ident compare1)

pattern DataFunctor :: ModuleName
pattern DataFunctor = ModuleName "Data.Functor"

pattern Functor :: Qualified (ProperName 'ClassName)
pattern Functor = Qualified (Just DataFunctor) (ProperName "Functor")

identMap :: Qualified Ident
identMap = Qualified (Just DataFunctor) (Ident map)

pattern DataSemiring :: ModuleName
pattern DataSemiring = ModuleName "Data.Semiring"

pattern DataRing :: ModuleName
pattern DataRing = ModuleName "Data.Ring"

pattern DataEuclideanRing :: ModuleName
pattern DataEuclideanRing = ModuleName "Data.EuclideanRing"

pattern DataFunction :: ModuleName
pattern DataFunction = ModuleName "Data.Function"

pattern DataIntBits :: ModuleName
pattern DataIntBits = ModuleName "Data.Int.Bits"

unsafePartial :: forall a. (IsString a) => a
unsafePartial = "unsafePartial"

pattern UnsafeCoerce :: ModuleName
pattern UnsafeCoerce = ModuleName "Unsafe.Coerce"

unsafeCoerceFn :: forall a. (IsString a) => a
unsafeCoerceFn = "unsafeCoerce"
