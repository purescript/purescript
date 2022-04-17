-- | Various constants which refer to things in Prim
module Language.PureScript.Constants.Prim where

import Data.String (IsString)
import Language.PureScript.Names

-- Prim values

undefined :: forall a. (IsString a) => a
undefined = "undefined"

-- Prim

pattern Prim :: ModuleName
pattern Prim = ModuleName "Prim"

pattern Partial :: Qualified (ProperName 'ClassName)
pattern Partial = Qualified (ByModuleName Prim) (ProperName "Partial")

pattern Record :: Qualified (ProperName 'TypeName)
pattern Record = Qualified (ByModuleName Prim) (ProperName "Record")

pattern Type :: Qualified (ProperName 'TypeName)
pattern Type = Qualified (ByModuleName Prim) (ProperName "Type")

pattern Constraint :: Qualified (ProperName 'TypeName)
pattern Constraint = Qualified (ByModuleName Prim) (ProperName "Constraint")

pattern Function :: Qualified (ProperName 'TypeName)
pattern Function = Qualified (ByModuleName Prim) (ProperName "Function")

pattern Array :: Qualified (ProperName 'TypeName)
pattern Array = Qualified (ByModuleName Prim) (ProperName "Array")

pattern Row :: Qualified (ProperName 'TypeName)
pattern Row = Qualified (ByModuleName Prim) (ProperName "Row")

-- Prim.Boolean

pattern PrimBoolean :: ModuleName
pattern PrimBoolean = ModuleName "Prim.Boolean"

booleanTrue :: Qualified (ProperName 'TypeName)
booleanTrue = Qualified (ByModuleName PrimBoolean) (ProperName "True")

booleanFalse :: Qualified (ProperName 'TypeName)
booleanFalse = Qualified (ByModuleName PrimBoolean) (ProperName "False")

-- Prim.Coerce

pattern PrimCoerce :: ModuleName
pattern PrimCoerce = ModuleName "Prim.Coerce"

pattern Coercible :: Qualified (ProperName 'ClassName)
pattern Coercible = Qualified (ByModuleName PrimCoerce) (ProperName "Coercible")

-- Prim.Ordering

pattern PrimOrdering :: ModuleName
pattern PrimOrdering = ModuleName "Prim.Ordering"

orderingLT :: Qualified (ProperName 'TypeName)
orderingLT = Qualified (ByModuleName PrimOrdering) (ProperName "LT")

orderingEQ :: Qualified (ProperName 'TypeName)
orderingEQ = Qualified (ByModuleName PrimOrdering) (ProperName "EQ")

orderingGT :: Qualified (ProperName 'TypeName)
orderingGT = Qualified (ByModuleName PrimOrdering) (ProperName "GT")

-- Prim.Row

pattern PrimRow :: ModuleName
pattern PrimRow = ModuleName "Prim.Row"

pattern RowUnion :: Qualified (ProperName 'ClassName)
pattern RowUnion = Qualified (ByModuleName PrimRow) (ProperName "Union")

pattern RowNub :: Qualified (ProperName 'ClassName)
pattern RowNub = Qualified (ByModuleName PrimRow) (ProperName "Nub")

pattern RowCons :: Qualified (ProperName 'ClassName)
pattern RowCons = Qualified (ByModuleName PrimRow) (ProperName "Cons")

pattern RowLacks :: Qualified (ProperName 'ClassName)
pattern RowLacks = Qualified (ByModuleName PrimRow) (ProperName "Lacks")

-- Prim.RowList

pattern PrimRowList :: ModuleName
pattern PrimRowList = ModuleName "Prim.RowList"

pattern RowToList :: Qualified (ProperName 'ClassName)
pattern RowToList = Qualified (ByModuleName PrimRowList) (ProperName "RowToList")

pattern RowListNil :: Qualified (ProperName 'TypeName)
pattern RowListNil = Qualified (ByModuleName PrimRowList) (ProperName "Nil")

pattern RowListCons :: Qualified (ProperName 'TypeName)
pattern RowListCons = Qualified (ByModuleName PrimRowList) (ProperName "Cons")

-- Prim.Int

pattern PrimInt :: ModuleName
pattern PrimInt = ModuleName "Prim.Int"

pattern IntAdd :: Qualified (ProperName 'ClassName)
pattern IntAdd = Qualified (ByModuleName PrimInt) (ProperName "Add")

pattern IntCompare :: Qualified (ProperName 'ClassName)
pattern IntCompare = Qualified (ByModuleName PrimInt) (ProperName "Compare")

pattern IntMul :: Qualified (ProperName 'ClassName)
pattern IntMul = Qualified (ByModuleName PrimInt) (ProperName "Mul")

-- Prim.Symbol

pattern PrimSymbol :: ModuleName
pattern PrimSymbol = ModuleName "Prim.Symbol"

pattern SymbolCompare :: Qualified (ProperName 'ClassName)
pattern SymbolCompare = Qualified (ByModuleName PrimSymbol) (ProperName "Compare")

pattern SymbolAppend :: Qualified (ProperName 'ClassName)
pattern SymbolAppend = Qualified (ByModuleName PrimSymbol) (ProperName "Append")

pattern SymbolCons :: Qualified (ProperName 'ClassName)
pattern SymbolCons = Qualified (ByModuleName PrimSymbol) (ProperName "Cons")

-- Prim.TypeError

pattern PrimTypeError :: ModuleName
pattern PrimTypeError = ModuleName "Prim.TypeError"

pattern Fail :: Qualified (ProperName 'ClassName)
pattern Fail = Qualified (ByModuleName PrimTypeError) (ProperName "Fail")

pattern Warn :: Qualified (ProperName 'ClassName)
pattern Warn = Qualified (ByModuleName PrimTypeError) (ProperName "Warn")

primModules :: [ModuleName]
primModules = [Prim, PrimBoolean, PrimCoerce, PrimOrdering, PrimRow, PrimRowList, PrimSymbol, PrimInt, PrimTypeError]

typ :: forall a. (IsString a) => a
typ = "Type"

kindOrdering :: forall a. (IsString a) => a
kindOrdering = "Ordering"

kindRowList :: forall a. (IsString a) => a
kindRowList = "RowList"

symbol :: forall a. (IsString a) => a
symbol = "Symbol"

doc :: forall a. (IsString a) => a
doc = "Doc"

row :: forall a. (IsString a) => a
row = "Row"

constraint :: forall a. (IsString a) => a
constraint = "Constraint"

-- Modules

prim :: forall a. (IsString a) => a
prim = "Prim"

moduleBoolean :: forall a. (IsString a) => a
moduleBoolean = "Boolean"

moduleCoerce :: forall a. (IsString a) => a
moduleCoerce = "Coerce"

moduleOrdering :: forall a. (IsString a) => a
moduleOrdering = "Ordering"

moduleRow :: forall a. (IsString a) => a
moduleRow = "Row"

moduleRowList :: forall a. (IsString a) => a
moduleRowList = "RowList"

moduleSymbol :: forall a. (IsString a) => a
moduleSymbol = "Symbol"

moduleInt :: forall a. (IsString a) => a
moduleInt = "Int"

typeError :: forall a. (IsString a) => a
typeError = "TypeError"
