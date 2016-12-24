-- | This module provides documentation for the builtin Prim module.
module Language.PureScript.Docs.Prim (primDocsModule) where

import Prelude.Compat hiding (fail)
import Control.Arrow (first)
import qualified Data.Text as T
import qualified Data.Map as Map
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P

primDocsModule :: Module
primDocsModule = Module
  { modName = P.moduleNameFromString "Prim"
  , modComments = Just "The Prim module is embedded in the PureScript compiler in order to provide compiler support for certain types &mdash; for example, value literals, or syntax sugar."
  , modDeclarations =
      [ function
      , array
      , record
      , number
      , int
      , string
      , char
      , boolean
      , partial
      , fail
      , typeConcat
      , typeString
      ]
  , modReExports = []
  }

unsafeLookup :: forall v (a :: P.ProperNameType).
  Map.Map (P.Qualified (P.ProperName a)) v -> String -> String -> v
unsafeLookup m errorMsg ty = go ty
  where
  go = fromJust' . flip Map.lookup m . P.primName . T.pack

  fromJust' (Just x) = x
  fromJust' _ = P.internalError $ errorMsg ++ ty

lookupPrimKind :: String -> P.Kind
lookupPrimKind = fst . unsafeLookup P.primTypes "Docs.Prim: No such Prim type: "

primType :: String -> String ->  Declaration
primType title comments = Declaration
  { declTitle = title
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo = ExternDataDeclaration (lookupPrimKind title)
  }

-- | Lookup the TypeClassData of a Prim class. This function is specifically
-- not exported because it is partial.
lookupPrimClass :: String -> P.TypeClassData
lookupPrimClass = unsafeLookup P.primClasses "Docs.Prim: No such Prim class: "

primClass :: String -> String -> Declaration
primClass title comments = Declaration
  { declTitle = title
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo =
      let
        tcd = lookupPrimClass title
        args = P.typeClassArguments tcd
        superclasses = P.typeClassSuperclasses tcd
        fundeps = convertFundepsToStrings args (P.typeClassDependencies tcd)
      in
        TypeClassDeclaration (map (first T.unpack) args) superclasses fundeps
  }

function :: Declaration
function = primType "Function" $ unlines
  [ "A function, which takes values of the type specified by the first type"
  , "parameter, and returns values of the type specified by the second."
  , "In the JavaScript backend, this is a standard JavaScript Function."
  , ""
  , "The type constructor `(->)` is syntactic sugar for this type constructor."
  , "It is recommended to use `(->)` rather than `Function`, where possible."
  , ""
  , "That is, prefer this:"
  , ""
  , "    f :: Number -> Number"
  , ""
  , "to either of these:"
  , ""
  , "    f :: Function Number Number"
  , "    f :: (->) Number Number"
  ]

array :: Declaration
array = primType "Array" $ unlines
  [ "An Array: a data structure supporting efficient random access. In"
  , "the JavaScript backend, values of this type are represented as JavaScript"
  , "Arrays at runtime."
  , ""
  , "Construct values using literals:"
  , ""
  , "    x = [1,2,3,4,5] :: Array Int"
  ]

record :: Declaration
record = primType "Record" $ unlines
  [ "The type of records whose fields are known at compile time. In the"
  , "JavaScript backend, values of this type are represented as JavaScript"
  , "Objects at runtime."
  , ""
  , "The type signature here means that the `Record` type constructor takes"
  , "a row of concrete types. For example:"
  , ""
  , "    type Person = Record (name :: String, age :: Number)"
  , ""
  , "The syntactic sugar with curly braces `{ }` is generally preferred, though:"
  , ""
  , "    type Person = { name :: String, age :: Number }"
  ]

number :: Declaration
number = primType "Number" $ unlines
  [ "A double precision floating point number (IEEE 754)."
  , ""
  , "Construct values of this type with literals:"
  , ""
  , "    y = 35.23 :: Number"
  , "    z = 1.224e6 :: Number"
  ]

int :: Declaration
int = primType "Int" $ unlines
  [ "A 32-bit signed integer. See the purescript-integers package for details"
  , "of how this is accomplished when compiling to JavaScript."
  , ""
  , "Construct values of this type with literals:"
  , ""
  , "    x = 23 :: Int"
  ]

string :: Declaration
string = primType "String" $ unlines
  [ "A String. As in JavaScript, String values represent sequences of UTF-16"
  , "code units, which are not required to form a valid encoding of Unicode"
  , "text (for example, lone surrogates are permitted)."
  , ""
  , "Construct values of this type with literals, using double quotes `\"`:"
  , ""
  , "    x = \"hello, world\" :: String"
  , ""
  , "Multi-line string literals are also supported with triple quotes (`\"\"\"`)."
  ]

char :: Declaration
char = primType "Char" $ unlines
   [ "A single character (UTF-16 code unit). The JavaScript representation is a"
   , "normal String, which is guaranteed to contain one code unit. This means"
   , "that astral plane characters (i.e. those with code point values greater"
   , "than 0xFFFF) cannot be represented as Char values."
   , ""
   , "Construct values of this type with literals, using single quotes `'`:"
   , ""
   , "    x = 'a' :: Char"
   ]

boolean :: Declaration
boolean = primType "Boolean" $ unlines
  [ "A JavaScript Boolean value."
  , ""
  , "Construct values of this type with the literals `true` and `false`."
  ]

partial :: Declaration
partial = primClass "Partial" $ unlines
  [ "The Partial type class is used to indicate that a function is *partial,*"
  , "that is, it will throw an error for some inputs. For more information,"
  , "see [the Partial type class guide](https://github.com/purescript/documentation/blob/master/guides/The-Partial-type-class.md)."
  ]

fail :: Declaration
fail = primClass "Fail" $ unlines
  [ "The Fail type class is part of the custom type errors feature. To provide"
  , "a custom type error when someone tries to use a particular instance,"
  , "write that instance out with a Fail constraint."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/paf31/24-days-of-purescript-2016/blob/master/21.markdown)."
  ]

typeConcat :: Declaration
typeConcat = primType "TypeConcat" $ unlines
  [ "The TypeConcat type constructor concatenates two Symbols in a custom type"
  , "error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/paf31/24-days-of-purescript-2016/blob/master/21.markdown)."
  ]

typeString :: Declaration
typeString = primType "TypeString" $ unlines
  [ "The TypeString type constructor renders any concrete type into a Symbol"
  , "in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/paf31/24-days-of-purescript-2016/blob/master/21.markdown)."
  ]
