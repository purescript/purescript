-- | This module provides documentation for the builtin Prim modules.
module Language.PureScript.Docs.Prim
  ( primDocsModule
  , primRowDocsModule
  , primTypeErrorDocsModule
  , primModules
  ) where

import Prelude hiding (fail)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map qualified as Map
import Language.PureScript.Docs.Types (Declaration(..), DeclarationInfo(..), Module(..), Type', convertFundepsToStrings)

import Language.PureScript.Constants.Prim qualified as P
import Language.PureScript.Crash qualified as P
import Language.PureScript.Environment qualified as P
import Language.PureScript.Names qualified as P

primModules :: [Module]
primModules =
  [ primDocsModule
  , primBooleanDocsModule
  , primCoerceDocsModule
  , primOrderingDocsModule
  , primRowDocsModule
  , primRowListDocsModule
  , primSymbolDocsModule
  , primIntDocsModule
  , primTypeErrorDocsModule
  ]

primDocsModule :: Module
primDocsModule = Module
  { modName = P.moduleNameFromString "Prim"
  , modComments = Just $ T.unlines
      [ "The `Prim` module is embedded in the PureScript compiler in order to provide compiler support for certain types &mdash; for example, value literals, or syntax sugar. It is implicitly imported unqualified in every module except those that list it as a qualified import."
      , ""
      , "`Prim` does not include additional built-in types and kinds that are defined deeper in the compiler such as Type wildcards (e.g. `f :: _ -> Int`) and Quantified Types. Rather, these are documented in [the PureScript language reference](https://github.com/purescript/documentation/blob/master/language/Types.md)."
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
      , kindConstraint
      , kindSymbol
      , kindRow
      ]
  , modReExports = []
  }

primBooleanDocsModule :: Module
primBooleanDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Boolean"
  , modComments = Just "The Prim.Boolean module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains a type level `Boolean` data structure."
  , modDeclarations =
      [ booleanTrue
      , booleanFalse
      ]
  , modReExports = []
  }

primCoerceDocsModule :: Module
primCoerceDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Coerce"
  , modComments = Just "The Prim.Coerce module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains an automatically solved type class for coercing types that have provably-identical runtime representations with [purescript-safe-coerce](https://pursuit.purescript.org/packages/purescript-safe-coerce)."
  , modDeclarations =
      [ coercible
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

primIntDocsModule :: Module
primIntDocsModule = Module
  { modName = P.moduleNameFromString "Prim.Int"
  , modComments = Just "The Prim.Int module is embedded in the PureScript compiler. Unlike `Prim`, it is not imported implicitly. It contains automatically solved type classes for working with type-level intural numbers."
  , modDeclarations =
      [ intAdd
      , intCompare
      , intMul
      , intToString
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

unsafeLookup
  :: forall v (a :: P.ProperNameType)
  . Map.Map (P.Qualified (P.ProperName a)) v
  -> String
  -> P.Qualified (P.ProperName a)
  -> v
unsafeLookup m errorMsg name = go name
  where
  go = fromJust' . flip Map.lookup m

  fromJust' (Just x) = x
  fromJust' _ = P.internalError $ errorMsg ++ show (P.runProperName $ P.disqualify name)

lookupPrimTypeKind
  :: P.Qualified (P.ProperName 'P.TypeName)
  -> Type'
lookupPrimTypeKind = ($> ()) . fst . unsafeLookup
  ( P.primTypes <>
    P.primBooleanTypes <>
    P.primOrderingTypes <>
    P.primRowTypes <>
    P.primRowListTypes <>
    P.primTypeErrorTypes
  ) "Docs.Prim: No such Prim type: "

primType :: P.Qualified (P.ProperName 'P.TypeName) -> Text -> Declaration
primType tn comments = Declaration
  { declTitle = P.runProperName $ P.disqualify tn
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo = ExternDataDeclaration (lookupPrimTypeKind tn) []
  , declKind = Nothing
  }

-- | Lookup the TypeClassData of a Prim class. This function is specifically
-- not exported because it is partial.
lookupPrimClass :: P.Qualified (P.ProperName 'P.ClassName) -> P.TypeClassData
lookupPrimClass = unsafeLookup
  ( P.primClasses <>
    P.primCoerceClasses <>
    P.primRowClasses <>
    P.primRowListClasses <>
    P.primSymbolClasses <>
    P.primIntClasses <>
    P.primTypeErrorClasses
  ) "Docs.Prim: No such Prim class: "

primClass :: P.Qualified (P.ProperName 'P.ClassName) -> Text -> Declaration
primClass cn comments = Declaration
  { declTitle = P.runProperName $ P.disqualify cn
  , declComments = Just comments
  , declSourceSpan = Nothing
  , declChildren = []
  , declInfo =
      let
        tcd = lookupPrimClass cn
        args = fmap (fmap ($> ())) <$> P.typeClassArguments tcd
        superclasses = ($> ()) <$> P.typeClassSuperclasses tcd
        fundeps = convertFundepsToStrings args (P.typeClassDependencies tcd)
      in
        TypeClassDeclaration args superclasses fundeps
  , declKind = Nothing
  }

kindType :: Declaration
kindType = primType P.Type $ T.unlines
  [ "`Type` is the kind of all proper types: those that classify value-level terms."
  , "For example the type `Boolean` has kind `Type`; denoted by `Boolean :: Type`."
  ]

kindConstraint :: Declaration
kindConstraint = primType P.Constraint $ T.unlines
  [ "`Constraint` is the kind of type class constraints."
  , "For example, a type class declaration like this:"
  , ""
  , "    class Semigroup a where"
  , "      append :: a -> a -> a"
  , ""
  , "has the kind signature:"
  , ""
  , "    class Semigroup :: Type -> Constraint"
  ]

kindSymbol :: Declaration
kindSymbol = primType P.Symbol $ T.unlines
  [ "`Symbol` is the kind of type-level strings."
  , ""
  , "Construct types of this kind using the same literal syntax as documented"
  , "for strings."
  , ""
  , "    type Hello :: Symbol"
  , "    type Hello = \"Hello, world\""
  , ""
  ]

kindRow :: Declaration
kindRow = primType P.Row $ T.unlines
  [ "`Row` is the kind constructor of label-indexed types which map type-level strings to other types."
  , "The most common use of `Row` is `Row Type`, a row mapping labels to basic (of kind `Type`) types:"
  , ""
  , "    type ExampleRow :: Row Type"
  , "    type ExampleRow = ( name :: String, values :: Array Int )"
  , ""
  , "This is the kind of `Row` expected by the `Record` type constructor."
  , "More advanced row kinds like `Row (Type -> Type)` are used much less frequently."
  ]

function :: Declaration
function = primType P.Function $ T.unlines
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
array = primType P.Array $ T.unlines
  [ "An Array: a data structure supporting efficient random access. In"
  , "the JavaScript backend, values of this type are represented as JavaScript"
  , "Arrays at runtime."
  , ""
  , "Construct values using literals:"
  , ""
  , "    x = [1,2,3,4,5] :: Array Int"
  ]

record :: Declaration
record = primType P.Record $ T.unlines
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
number = primType P.Number $ T.unlines
  [ "A double precision floating point number (IEEE 754)."
  , ""
  , "Construct values of this type with literals."
  , "Negative literals must be wrapped in parentheses if the negation sign could be mistaken"
  , "for an infix operator:"
  , ""
  , "    x = 35.23 :: Number"
  , "    y = -1.224e6 :: Number"
  , "    z = exp (-1.0) :: Number"
  ]

int :: Declaration
int = primType P.Int $ T.unlines
  [ "A 32-bit signed integer. See the `purescript-integers` package for details"
  , "of how this is accomplished when compiling to JavaScript."
  , ""
  , "Construct values of this type with literals. Hexadecimal syntax is supported."
  , "Negative literals must be wrapped in parentheses if the negation sign could be mistaken"
  , "for an infix operator:"
  , ""
  , "    x = -23 :: Int"
  , "    y = 0x17 :: Int"
  , "    z = complement (-24) :: Int"
  , ""
  , "Integers used as types are considered to have kind `Int`."
  , "Unlike value-level `Int`s, which must be representable as a 32-bit signed integer,"
  , "type-level `Int`s are unbounded. Hexadecimal support is also supported at the type level."
  , ""
  , "    type One :: Int"
  , "    type One = 1"
  , "    "
  , "    type Beyond32BitSignedInt :: Int"
  , "    type Beyond32BitSignedInt = 2147483648"
  , "    "
  , "    type HexInt :: Int"
  , "    type HexInt = 0x17"
  , ""
  , "Negative integer literals at the type level must be"
  , "wrapped in parentheses if the negation sign could be mistaken for an infix operator."
  , ""
  , "    type NegativeOne = -1"
  , "    foo :: Proxy (-1) -> ..."
  ]

string :: Declaration
string = primType P.String $ T.unlines
  [ "A String. As in JavaScript, String values represent sequences of UTF-16"
  , "code units, which are not required to form a valid encoding of Unicode"
  , "text (for example, lone surrogates are permitted)."
  , ""
  , "Construct values of this type with literals, using double quotes `\"`:"
  , ""
  , "    x = \"hello, world\" :: String"
  , ""
  , "Multi-line string literals are also supported with triple quotes (`\"\"\"`):"
  , ""
  , "    x = \"\"\"multi"
  , "       line\"\"\""
  , ""
  , "At the type level, string literals represent types with kind `Symbol`."
  , "These types will have kind `String` in a future release:"
  , ""
  , "    type Hello :: Symbol"
  , "    type Hello = \"Hello, world\""
  ]

char :: Declaration
char = primType P.Char $ T.unlines
  [ "A single character (UTF-16 code unit). The JavaScript representation is a"
  , "normal `String`, which is guaranteed to contain one code unit. This means"
  , "that astral plane characters (i.e. those with code point values greater"
  , "than `0xFFFF`) cannot be represented as `Char` values."
  , ""
  , "Construct values of this type with literals, using single quotes `'`:"
  , ""
  , "    x = 'a' :: Char"
  ]

boolean :: Declaration
boolean = primType P.Boolean $ T.unlines
  [ "A JavaScript Boolean value."
  , ""
  , "Construct values of this type with the literals `true` and `false`."
  , ""
  , "The `True` and `False` types defined in `Prim.Boolean` have this type as their kind."
  ]

partial :: Declaration
partial = primClass P.Partial $ T.unlines
  [ "The Partial type class is used to indicate that a function is *partial,*"
  , "that is, it is not defined for all inputs. In practice, attempting to use"
  , "a partial function with a bad input will usually cause an error to be"
  , "thrown, although it is not safe to assume that this will happen in all"
  , "cases. For more information, see"
  , "[purescript-partial](https://pursuit.purescript.org/packages/purescript-partial/)."
  ]

booleanTrue :: Declaration
booleanTrue = primType P.True $ T.unlines
  [ "The 'True' boolean type."
  ]

booleanFalse :: Declaration
booleanFalse = primType P.False $ T.unlines
  [ "The 'False' boolean type."
  ]

coercible :: Declaration
coercible = primClass P.Coercible $ T.unlines
  [ "Coercible is a two-parameter type class that has instances for types `a`"
  , "and `b` if the compiler can infer that they have the same representation."
  , "Coercible constraints are solved according to the following rules:"
  , ""
  , "* _reflexivity_, any type has the same representation as itself:"
  , "`Coercible a a` holds."
  , ""
  , "* _symmetry_, if a type `a` can be coerced to some other type `b`, then `b`"
  , "can also be coerced back to `a`: `Coercible a b` implies `Coercible b a`."
  , ""
  , "* _transitivity_, if a type `a` can be coerced to some other type `b` which"
  , "can be coerced to some other type `c`, then `a` can also be coerced to `c`:"
  , "`Coercible a b` and `Coercible b c` imply `Coercible a c`."
  , ""
  , "* Newtypes can be freely wrapped and unwrapped when their constructor is"
  , "in scope:"
  , ""
  , "      newtype Age = Age Int"
  , ""
  , "`Coercible Int Age` and `Coercible Age Int` hold since `Age` has the same"
  , "runtime representation than `Int`."
  , ""
  , "Newtype constructors have to be in scope to preserve abstraction. It's"
  , "common to declare a newtype to encode some invariants (non emptiness of"
  , "arrays with `Data.Array.NonEmpty.NonEmptyArray` for example), hide its"
  , "constructor and export smart constructors instead. Without this restriction,"
  , "the guarantees provided by such newtypes would be void."
  , ""
  , "* If none of the above are applicable, two types of kind `Type` may be"
  , "coercible, but only if their heads are the same. For example,"
  , "`Coercible (Maybe a) (Either a b)` does not hold because `Maybe` and"
  , "`Either` are different. Those types don't share a common runtime"
  , "representation so coercing between them would be unsafe. In addition their"
  , "arguments may need to be identical or coercible, depending on the _roles_"
  , "of the head's type parameters. Roles are documented in [the PureScript"
  , "language reference](https://github.com/purescript/documentation/blob/master/language/Roles.md)."
  , ""
  , "Coercible being polykinded, we can also coerce more than types of kind `Type`:"
  , ""
  , "* Rows are coercible when they have the same labels, when the corresponding"
  , "pairs of types are coercible and when their tails are coercible:"
  , "`Coercible ( label :: a | r ) ( label :: b | s )` holds when"
  , "`Coercible a b` and `Coercible r s` do. Closed rows cannot be coerced to"
  , "open rows."
  , ""
  , "* Higher kinded types are coercible if they are coercible when fully"
  , "saturated: `Coercible (f :: _ -> Type) (g :: _ -> Type)` holds when"
  , "`Coercible (f a) (g a)` does."
  , ""
  , "This rule may seem puzzling since there is no term of type `_ -> Type` to"
  , "apply `coerce` to, but it is necessary when coercing types with higher"
  , "kinded parameters."
  ]

kindOrdering :: Declaration
kindOrdering = primType P.TypeOrdering $ T.unlines
  [ "The `Ordering` kind represents the three possibilities of comparing two"
  , "types of the same kind: `LT` (less than), `EQ` (equal to), and"
  , "`GT` (greater than)."
  ]

orderingLT :: Declaration
orderingLT = primType P.LT $ T.unlines
  [ "The 'less than' ordering type."
  ]

orderingEQ :: Declaration
orderingEQ = primType P.EQ $ T.unlines
  [ "The 'equal to' ordering type."
  ]

orderingGT :: Declaration
orderingGT = primType P.GT $ T.unlines
  [ "The 'greater than' ordering type."
  ]

union :: Declaration
union = primClass P.RowUnion $ T.unlines
  [ "The Union type class is used to compute the union of two rows of types"
  , "(left-biased, including duplicates)."
  , ""
  , "The third type argument represents the union of the first two."
  ]

nub :: Declaration
nub = primClass P.RowNub $ T.unlines
  [ "The Nub type class is used to remove duplicate labels from rows."
  ]

lacks :: Declaration
lacks = primClass P.RowLacks $ T.unlines
  [ "The Lacks type class asserts that a label does not occur in a given row."
  ]

rowCons :: Declaration
rowCons = primClass P.RowCons $ T.unlines
  [ "The Cons type class is a 4-way relation which asserts that one row of"
  , "types can be obtained from another by inserting a new label/type pair on"
  , "the left."
  ]

kindRowList :: Declaration
kindRowList = primType P.RowList $ T.unlines
  [ "A type level list representation of a row of types."
  ]

rowListCons :: Declaration
rowListCons = primType P.RowListCons $ T.unlines
  [ "Constructs a new `RowList` from a label, a type, and an existing tail"
  , "`RowList`.  E.g: `Cons \"x\" Int (Cons \"y\" Int Nil)`."
  ]

rowListNil :: Declaration
rowListNil = primType P.RowListNil $ T.unlines
  [ "The empty `RowList`."
  ]

rowToList :: Declaration
rowToList = primClass P.RowToList $ T.unlines
  [ "Compiler solved type class for generating a `RowList` from a closed row"
  , "of types.  Entries are sorted by label and duplicates are preserved in"
  , "the order they appeared in the row."
  ]

symbolAppend :: Declaration
symbolAppend = primClass P.SymbolAppend $ T.unlines
  [ "Compiler solved type class for appending `Symbol`s together."
  ]

symbolCompare :: Declaration
symbolCompare = primClass P.SymbolCompare $ T.unlines
  [ "Compiler solved type class for comparing two `Symbol`s."
  , "Produces an `Ordering`."
  ]

symbolCons :: Declaration
symbolCons = primClass P.SymbolCons $ T.unlines
  [ "Compiler solved type class for either splitting up a symbol into its"
  , "head and tail or for combining a head and tail into a new symbol."
  , "Requires the head to be a single character and the combined string"
  , "cannot be empty."
  ]

intAdd :: Declaration
intAdd = primClass P.IntAdd $ T.unlines
  [ "Compiler solved type class for adding type-level `Int`s."
  ]

intCompare :: Declaration
intCompare = primClass P.IntCompare $ T.unlines
  [ "Compiler solved type class for comparing two type-level `Int`s."
  , "Produces an `Ordering`."
  ]

intMul :: Declaration
intMul = primClass P.IntMul $ T.unlines
  [ "Compiler solved type class for multiplying type-level `Int`s."
  ]

intToString :: Declaration
intToString = primClass P.IntToString $ T.unlines
  [ "Compiler solved type class for converting a type-level `Int` into a type-level `String` (i.e. `Symbol`)."
  ]

fail :: Declaration
fail = primClass P.Fail $ T.unlines
  [ "The Fail type class is part of the custom type errors feature. To provide"
  , "a custom type error when someone tries to use a particular instance,"
  , "write that instance out with a Fail constraint."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

warn :: Declaration
warn = primClass P.Warn $ T.unlines
  [ "The Warn type class allows a custom compiler warning to be displayed."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

kindDoc :: Declaration
kindDoc = primType P.Doc $ T.unlines
  [ "`Doc` is the kind of type-level documents."
  , ""
  , "This kind is used with the `Fail` and `Warn` type classes."
  , "Build up a `Doc` with `Text`, `Quote`, `QuoteLabel`, `Beside`, and `Above`."
  ]

textDoc :: Declaration
textDoc = primType P.Text $ T.unlines
  [ "The Text type constructor makes a Doc from a Symbol"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

quoteDoc :: Declaration
quoteDoc = primType P.Quote $ T.unlines
  [ "The Quote type constructor renders any concrete type as a Doc"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

quoteLabelDoc :: Declaration
quoteLabelDoc = primType P.QuoteLabel $ T.unlines
  [ "The `QuoteLabel` type constructor will produce a `Doc` when given a `Symbol`. When the resulting `Doc` is rendered"
  , "for a `Warn` or `Fail` constraint, a syntactically valid label will be produced, escaping with quotes as needed."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

besideDoc :: Declaration
besideDoc = primType P.Beside $ T.unlines
  [ "The Beside type constructor combines two Docs horizontally"
  , "to be used in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]

aboveDoc :: Declaration
aboveDoc = primType P.Above $ T.unlines
  [ "The Above type constructor combines two Docs vertically"
  , "in a custom type error."
  , ""
  , "For more information, see"
  , "[the Custom Type Errors guide](https://github.com/purescript/documentation/blob/master/guides/Custom-Type-Errors.md)."
  ]
