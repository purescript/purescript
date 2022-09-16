module Language.PureScript.Constants.Data.Debug where

import Data.String (IsString)
import Language.PureScript.Names

pattern DataDebug :: ModuleName
pattern DataDebug = ModuleName "Data.Debug"

pattern Debug :: Qualified (ProperName 'ClassName)
pattern Debug = Qualified (ByModuleName DataDebug) (ProperName "Debug")

debug :: forall a. (IsString a) => a
debug = "debug"

identDebug :: Qualified Ident
identDebug = Qualified (ByModuleName DataDebug) (Ident debug)

pattern DataDebugType :: ModuleName
pattern DataDebugType = ModuleName "Data.Debug.Type"

int :: forall a. (IsString a) => a
int = "int"

identInt :: Qualified Ident
identInt = Qualified (ByModuleName DataDebugType) (Ident int)

number :: forall a. (IsString a) => a
number = "number"

identNumber :: Qualified Ident
identNumber = Qualified (ByModuleName DataDebugType) (Ident number)

boolean :: forall a. (IsString a) => a
boolean = "boolean"

identBoolean :: Qualified Ident
identBoolean = Qualified (ByModuleName DataDebugType) (Ident boolean)

char :: forall a. (IsString a) => a
char = "char"

identChar :: Qualified Ident
identChar = Qualified (ByModuleName DataDebugType) (Ident char)

string :: forall a. (IsString a) => a
string = "string"

identString :: Qualified Ident
identString = Qualified (ByModuleName DataDebugType) (Ident string)

array :: forall a. (IsString a) => a
array = "array"

identArray :: Qualified Ident
identArray = Qualified (ByModuleName DataDebugType) (Ident array)

record :: forall a. (IsString a) => a
record = "record"

identRecord :: Qualified Ident
identRecord = Qualified (ByModuleName DataDebugType) (Ident record)

constructor :: forall a. (IsString a) => a
constructor = "constructor"

identConstructor :: Qualified Ident
identConstructor = Qualified (ByModuleName DataDebugType) (Ident constructor)

opaque :: forall a. (IsString a) => a
opaque = "opaque"

identOpaque :: Qualified Ident
identOpaque = Qualified (ByModuleName DataDebugType) (Ident opaque)

opaque_ :: forall a. (IsString a) => a
opaque_ = "opaque_"

identOpaque_ :: Qualified Ident
identOpaque_ = Qualified (ByModuleName DataDebugType) (Ident opaque_)
