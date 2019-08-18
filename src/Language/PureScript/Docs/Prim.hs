-- | This module provides documentation for the builtin Prim modules.
module Language.PureScript.Docs.Prim
  ( primDocsModule
  , primRowDocsModule
  , primTypeErrorDocsModule
  , primModules
  ) where

import Prelude.Compat hiding (fail)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.PureScript.Docs.Types

import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Names as P

primModules :: [Module]
primModules =
  [ primDocsModule
  , primBooleanDocsModule
  , primOrderingDocsModule
  , primRowDocsModule
  , primRowListDocsModule
  , primSymbolDocsModule
  , primTypeErrorDocsModule
  ]

primDocsModule :: Module
primDocsModule = Module
  { modName = P.moduleNameFromString "Prim"
  , modComments = Just $ T.unlines
      [ "The `Prim` module is embedded in the PureScript compiler in order to provide compiler support for certain types &mdash; for example, value literals, or syntax sugar. It is implicitly imported unqualified in every module except those that list it as a qualified import."
      , ""
      , "`Prim` does not include additional built-in types and kinds that are defined deeper in the compiler. For example, row kinds (e.g. `# Type`, which is the kind of types such as `(name :: String, age :: Int)`), Type wildcards (e.g. `f :: _ -> Int`), and Quantified Types. Rather, these are documented in [the PureScript language reference](https://github.com/purescript/documentation/blob/master/language/Types.md)."
      ]
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
      , kindType
      , kindSymbol
      ]
  , modReExports = []
  }

primBooleanDocsModule :: Module
primBooleanDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Boolean"
  , modComments = Just "The Prim.Boolean module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains a type level `Boolean` data structure."
  , modDeclarations =
      [ kindBoolean
      , booleanTrue
      , booleanFalse
      ]
  , modReExports = []
  }

primOrderingDocsModule :: Module
primOrderingDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Ordering"
  , modComments = Just "The Prim.Ordering module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains a type level `Ordering` data structure."
  , modDeclarations =
      [ kindOrdering
      , orderingLT
      , orderingEQ
      , orderingGT
      ]
  , modReExports = []
  }

primRowDocsModule :: Module
primRowDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Row"
  , modComments = Just "The Prim.Row module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains automatically solved type classes for working with row types."
  , modDeclarations =
      [ union
      , nub
      , lacks
      , rowCons
      ]
  , modReExports = []
  }

primRowListDocsModule :: Module
primRowListDocsModule = Module
  { modName = P.moduleNameFromString "Prim.RowList"
  , modComments = Just "The Prim.RowList module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains a type level list (`RowList`) that represents an ordered view of a row of types."
  , modDeclarations =
      [ kindRowList
      , rowListCons
      , rowListNil
      , rowToList
      ]
  , modReExports = []
  }

primSymbolDocsModule :: Module
primSymbolDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Symbol"
  , modComments = Just "The Prim.Symbol module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains automatically solved type classes for working with `Symbols`."
  , modDeclarations =
      [ symbolAppend
      , symbolCompare
      , symbolCons
      ]
  , modReExports = []
  }

primTypeErrorDocsModule :: Module
primTypeErrorDocsModule = Module
  { modName = P.moduleNameFromString "Prim.TypeError"
  , modComments = Just "The Prim.TypeError module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains type classes that provide custom type error and warning functionality."
  , modDeclarations =
      [ warn
      , fail
      , kindDoc
      , textDoc
      , quoteDoc
      , quoteLabelDoc
      , besideDoc
      , aboveDoc
      ]
  , modReExports = []
  }

type NameGen a = Text -> P.Qualified (P.ProperName a)

unsafeLookupOf
  :: forall v (a :: P.ProperNameType)
  . NameGen a
  -> Map.Map (P.Qualified (P.ProperName a)) v
  -> String
  -> Text
  -> v
unsafeLookupOf k m errorMsg name = go name
  where
  go = fromJust' . flip Map.lookup m . k

  fromJust' (Just x) = x
  fromJust' _ = P.internalError $ errorMsg ++ show name

primKindOf
  :: NameGen 'P.KindName
  -> Text
  -> Text
  -> Declaration
primKindOf g title comments =
  if Set.member (g title) P.allPrimKinds
     then Declaration
          { declTitle = title
          , declComments = Just comments
          , declSourceSpan = Nothing
          , declChildren = []
          , declInfo = ExternKindDeclaration
          }
    else P.internalError $ "Docs.Prim: No such Prim kind: " ++ T.unpack title

primKind :: Text -> Text -> Declaration
primKind = primKindOf P.primName

lookupPrimTypeKindOf
  :: NameGen 'P.TypeName
  -> Text
  -> Kind'
lookupPrimTypeKindOf k = ($> ()) . fst . unsafeLookupOf k
  ( P.primTypes <>
    P.primBooleanTypes <>
    P.primOrderingTypes <>
    P.primRowTypes <>
    P.primRowListTypes <>
    P.primTypeErrorTypes
  ) "Docs.Prim: No such Prim type: "

primType :: Text -> Text -> Declaration
primType = primTypeOf P.primName

primTypeOf :: NameGen 'P.TypeName -> Text -> Text -> Declaration
primTypeOf gen title comments = Declaration
  { declTitle = title
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo = ExternDataDeclaration (lookupPrimTypeKindOf gen title)
  }

-- | Lookup the TypeClassData of a Prim class. This function is specifically
-- not exported because it is partial.
lookupPrimClassOf :: NameGen 'P.ClassName -> Text -> P.TypeClassData
lookupPrimClassOf g = unsafeLookupOf g
  ( P.primClasses <>
    P.primRowClasses <>
    P.primRowListClasses <>
    P.primSymbolClasses <>
    P.primTypeErrorClasses
  ) "Docs.Prim: No such Prim class: "

primClass :: Text -> Text -> Declaration
primClass = primClassOf P.primName

primClassOf :: NameGen 'P.ClassName -> Text -> Text -> Declaration
primClassOf gen title comments = Declaration
  { declTitle = title
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo =
      let
        tcd = lookupPrimClassOf gen title
        args = fmap (fmap (fmap ($> ()))) $ P.typeClassArguments tcd
        superclasses = fmap ($> ()) $ P.typeClassSuperclasses tcd
        fundeps = convertFundepsToStrings args (P.typeClassDependencies tcd)
      in
        TypeClassDeclaration args superclasses fundeps
  }

kindType :: Declaration
kindType = primKind "Type" $ T.unlines
  [ "`Type` is the kind of all proper types: those that classify value-level terms."
  , "For example the type `Boolean` has kind `Type`; denoted by `Boolean :: Type`."
  ]

kindSymbol :: Declaration
kindSymbol = primKind "Symbol" $ T.unlines
  [ "`Symbol` is the kind of type-level strings."
  , ""
  , "Construct types of this kind using the same literal syntax as documented"
  , "for strings."
  ]

function :: Declaration
function = primType "Function" $ T.unlines
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
array = primType "Array" $ T.unlines
  [ "An Array: a data structure supporting efficient random access. In"
  , "the JavaScript backend, values of this type are represented as JavaScript"
  , "Arrays at runtime."
  , ""
  , "Construct values using literals:"
  , ""
  , "    x = [1,2,3,4,5] :: Array Int"
  ]

record :: Declaration
record = primType "Record" $ T.unlines
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
  , ""
  , "The row associates a type to each label which appears in the record."
  , ""
  , "_Technical note_: PureScript allows duplicate labels in rows, and the"
  , "meaning of `Record r` is based on the _first_ occurrence of each label in"
  , "the row `r`."
  ]

number :: Declaration
number = primType "Number" $ T.unlines
  [ "A double precision floating point number (IEEE 754)."
  , ""
  , "Construct values of this type with literals:"
  , ""
  , "    y = 35.23 :: Number"
  , "    z = 1.224e6 :: Number"
  ]

int :: Declaration
int = primType "Int" $ T.unlines
  [ "A 32-bit signed integer. See the purescript-integers package for details"
  , "of how this is accomplished when compiling to JavaScript."
  , ""
  , "Construct values of this type with literals:"
  , ""
  , "    x = 23 :: Int"
  ]

string :: Declaration
string = primType "String" $ T.unlines
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
char = primType "Char" $ T.unlines
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
boolean = primType "Boolean" $ T.unlines
  [ "A JavaScript Boolean value."
  , ""
  , "Construct values of this type with the literals `true` and `false`."
  ]

partial :: Declaration
partial = primClass "Partial" $ T.unlines
  [ "The Partial type class is used to indicate that a function is *partial,*"
  , "that is, it is not defined for all inputs. In practice, attempting to use"
  , "a partial function with a bad input will usually cause an error to be"
  , "thrown, although it is not safe to assume that this will happen in all"
  , "cases. For more information, see"
  , "[purescript-partial](https://pursuit.purescript.org/packages/purescript-partial/)."
  ]

kindBoolean :: Declaration
kindBoolean = primKindOf (P.primSubName "Boolean") "Boolean" $ T.unlines
  [ "The `Boolean` kind provides True/False types at the type level"
  ]

booleanTrue :: Declaration
booleanTrue = primTypeOf (P.primSubName "Boolean") "True" $ T.unlines
  [ "The 'True' boolean type."
  ]

booleanFalse :: Declaration
booleanFalse = primTypeOf (P.primSubName "Boolean") "False" $ T.unlines
  [ "The 'False' boolean type."
  ]

kindOrdering :: Declaration
kindOrdering = primKindOf (P.primSubName "Ordering") "Ordering" $ T.unlines
  [ "The `Ordering` kind represents the three possibilites of comparing two"
  , "types of the same kind: `LT` (less than), `EQ` (equal to), and"
  , "`GT` (greater than)."
  ]

orderingLT :: Declaration
orderingLT = primTypeOf (P.primSubName "Ordering") "LT" $ T.unlines
  [ "The 'less than' ordering type."
  ]

orderingEQ :: Declaration
orderingEQ = primTypeOf (P.primSubName "Ordering") "EQ" $ T.unlines
  [ "The 'equal to' ordering type."
  ]

orderingGT :: Declaration
orderingGT = primTypeOf (P.primSubName "Ordering") "GT" $ T.unlines
  [ "The 'greater than' ordering type."
  ]

union :: Declaration
union = primClassOf (P.primSubName "Row") "Union" $ T.unlines
  [ "The Union type class is used to compute the union of two rows of types"
  , "(left-biased, including duplicates)."
  , ""
  , "The third type argument represents the union of the first two."
  ]

nub :: Declaration
nub = primClassOf (P.primSubName "Row") "Nub" $ T.unlines
  [ "The Nub type class is used to remove duplicate labels from rows."
  ]

lacks :: Declaration
lacks = primClassOf (P.primSubName "Row") "Lacks" $ T.unlines
  [ "The Lacks type class asserts that a label does not occur in a given row."
  ]

rowCons :: Declaration
rowCons = primClassOf (P.primSubName "Row") "Cons" $ T.unlines
  [ "The Cons type class is a 4-way relation which asserts that one row of"
  , "types can be obtained from another by inserting a new label/type pair on"
  , "the left."
  ]

kindRowList :: Declaration
kindRowList = primKindOf (P.primSubName "RowList") "RowList" $ T.unlines
  [ "A type level list representation of a row of types."
  ]

rowListCons :: Declaration
rowListCons = primTypeOf (P.primSubName "RowList") "Cons" $ T.unlines
  [ "Constructs a new `RowList` from a label, a type, and an existing tail"
  , "`RowList`.  E.g: `Cons \"x\" Int (Cons \"y\" Int Nil)`."
  ]

rowListNil :: Declaration
rowListNil = primTypeOf (P.primSubName "RowList") "Nil" $ T.unlines
  [ "The empty `RowList`."
  ]

rowToList :: Declaration
rowToList = primClassOf (P.primSubName "RowList") "RowToList" $ T.unlines
  [ "Compiler solved type class for generating a `RowList` from a closed row"
  , "of types.  Entries are sorted by label and duplicates are preserved in"
  , "the order they appeared in the row."
  ]

symbolAppend :: Declaration
symbolAppend = primClassOf (P.primSubName "Symbol") "Append" $ T.unlines
  [ "Compiler solved type class for appending `Symbol`s together."
  ]

symbolCompare :: Declaration
symbolCompare = primClassOf (P.primSubName "Symbol") "Compare" $ T.unlines
  [ "Compiler solved type class for comparing two `Symbol`s."
  , "Produces an `Ordering`."
  ]

symbolCons :: Declaration
symbolCons = primClassOf (P.primSubName "Symbol") "Cons" $ T.unlines
  [ "Compiler solved type class for either splitting up a symbol into its"
  , "head and tail or for combining a head and tail into a new symbol."
  , "Requires the head to be a single character and the combined string"
  , "cannot be empty."
  ]

fail :: Declaration
fail = primClassOf (P.primSubName "TypeError") "Fail" $ T.unlines
  [ "The Fail type class is part of the custom type errors feature. To provide"
  , "a custom type error when someone tries to use a particular instance,"
  , "write that instance out with a Fail constraint."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

warn :: Declaration
warn = primClassOf (P.primSubName "TypeError") "Warn" $ T.unlines
  [ "The Warn type class allows a custom compiler warning to be displayed."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

kindDoc :: Declaration
kindDoc = primKindOf (P.primSubName "TypeError") "Doc" $ T.unlines
  [ "`Doc` is the kind of type-level documents."
  , ""
  , "This kind is used with the `Fail` and `Warn` type clases."
  , "Build up a `Doc` with `Text`, `Quote`, `QuoteLabel`, `Beside`, and `Above`."
  ]

textDoc :: Declaration
textDoc = primTypeOf (P.primSubName "TypeError") "Text" $ T.unlines
  [ "The Text type constructor makes a Doc from a Symbol"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

quoteDoc :: Declaration
quoteDoc = primTypeOf (P.primSubName "TypeError") "Quote" $ T.unlines
  [ "The Quote type constructor renders any concrete type as a Doc"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

quoteLabelDoc :: Declaration
quoteLabelDoc = primTypeOf (P.primSubName "TypeError") "QuoteLabel" $ T.unlines
  [ "The `QuoteLabel` type constructor will produce a `Doc` when given a `Symbol`. When the resulting `Doc` is rendered"
  , "for a `Warn` or `Fail` constraint, a syntactically valid label will be produced, escaping with quotes as needed."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

besideDoc :: Declaration
besideDoc = primTypeOf (P.primSubName "TypeError") "Beside" $ T.unlines
  [ "The Beside type constructor combines two Docs horizontally"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

aboveDoc :: Declaration
aboveDoc = primTypeOf (P.primSubName "TypeError") "Above" $ T.unlines
  [ "The Above type constructor combines two Docs vertically"
  , "in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]
