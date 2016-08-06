-- |
-- Data types for the intermediate simplified-Javascript AST
--
module Language.PureScript.CodeGen.JS.AST where

import Prelude.Compat

import Control.Monad.Identity

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Comments
import Language.PureScript.Traversals

-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  -- |
  -- Constructor
  --
  | JSNew
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division
  --
  | Divide
  -- |
  -- Remainder
  --
  | Modulus
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo
  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- |
  -- Bitwise right shift with zero-fill
  --
  | ZeroFillShiftRight
  deriving (Show, Eq)

-- |
-- Data type for simplified Javascript expressions
--
data JS
  -- |
  -- A numeric literal
  --
  = JSNumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- |
  -- A string literal
  --
  | JSStringLiteral (Maybe SourceSpan) String
  -- |
  -- A boolean literal
  --
  | JSBooleanLiteral (Maybe SourceSpan) Bool
  -- |
  -- A unary operator application
  --
  | JSUnary (Maybe SourceSpan) UnaryOperator JS
  -- |
  -- A binary operator application
  --
  | JSBinary (Maybe SourceSpan) BinaryOperator JS JS
  -- |
  -- An array literal
  --
  | JSArrayLiteral (Maybe SourceSpan) [JS]
  -- |
  -- An array indexer expression
  --
  | JSIndexer (Maybe SourceSpan) JS JS
  -- |
  -- An object literal
  --
  | JSObjectLiteral (Maybe SourceSpan) [(String, JS)]
  -- |
  -- An object property accessor expression
  --
  | JSAccessor (Maybe SourceSpan) String JS
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | JSFunction (Maybe SourceSpan) (Maybe String) [String] JS
  -- |
  -- Function application
  --
  | JSApp (Maybe SourceSpan) JS [JS]
  -- |
  -- Variable
  --
  | JSVar (Maybe SourceSpan) String
  -- |
  -- Conditional expression
  --
  | JSConditional (Maybe SourceSpan) JS JS JS
  -- |
  -- A block of expressions in braces
  --
  | JSBlock (Maybe SourceSpan) [JS]
  -- |
  -- A variable introduction and optional initialization
  --
  | JSVariableIntroduction (Maybe SourceSpan) String (Maybe JS)
  -- |
  -- A variable assignment
  --
  | JSAssignment (Maybe SourceSpan) JS JS
  -- |
  -- While loop
  --
  | JSWhile (Maybe SourceSpan) JS JS
  -- |
  -- For loop
  --
  | JSFor (Maybe SourceSpan) String JS JS JS
  -- |
  -- ForIn loop
  --
  | JSForIn (Maybe SourceSpan) String JS JS
  -- |
  -- If-then-else statement
  --
  | JSIfElse (Maybe SourceSpan) JS JS (Maybe JS)
  -- |
  -- Return statement
  --
  | JSReturn (Maybe SourceSpan) JS
  -- |
  -- Throw statement
  --
  | JSThrow (Maybe SourceSpan) JS
  -- |
  -- Type-Of operator
  --
  | JSTypeOf (Maybe SourceSpan) JS
  -- |
  -- InstanceOf test
  --
  | JSInstanceOf (Maybe SourceSpan) JS JS
  -- |
  -- Labelled statement
  --
  | JSLabel (Maybe SourceSpan) String JS
  -- |
  -- Break statement
  --
  | JSBreak (Maybe SourceSpan) String
  -- |
  -- Continue statement
  --
  | JSContinue (Maybe SourceSpan) String
  -- |
  -- Raw Javascript (generated when parsing fails for an inline foreign import declaration)
  --
  | JSRaw (Maybe SourceSpan) String
  -- |
  -- Commented Javascript
  --
  | JSComment (Maybe SourceSpan) [Comment] JS deriving (Show, Eq)

withSourceSpan :: SourceSpan -> JS -> JS
withSourceSpan withSpan = go
  where
  ss :: Maybe SourceSpan
  ss = Just withSpan

  go :: JS -> JS
  go (JSNumericLiteral _ n) = JSNumericLiteral ss n
  go (JSStringLiteral _ s) = JSStringLiteral ss s
  go (JSBooleanLiteral _ b) = JSBooleanLiteral ss b
  go (JSUnary _ op j) = JSUnary ss op j
  go (JSBinary _ op j1 j2) = JSBinary ss op j1 j2
  go (JSArrayLiteral _ js) = JSArrayLiteral ss js
  go (JSIndexer _ j1 j2) = JSIndexer ss j1 j2
  go (JSObjectLiteral _ js) = JSObjectLiteral ss js
  go (JSAccessor _ prop j) = JSAccessor ss prop j
  go (JSFunction _ name args j) = JSFunction ss name args j
  go (JSApp _ j js) = JSApp ss j js
  go (JSVar _ s) = JSVar ss s
  go (JSConditional _ j1 j2 j3) = JSConditional ss j1 j2 j3
  go (JSBlock _ js) = JSBlock ss js
  go (JSVariableIntroduction _ name j) = JSVariableIntroduction ss name j
  go (JSAssignment _ j1 j2) = JSAssignment ss j1 j2
  go (JSWhile _ j1 j2) = JSWhile ss j1 j2
  go (JSFor _ name j1 j2 j3) = JSFor ss name j1 j2 j3
  go (JSForIn _ name j1 j2) = JSForIn ss name j1 j2
  go (JSIfElse _ j1 j2 j3) = JSIfElse ss j1 j2 j3
  go (JSReturn _ js) = JSReturn ss js
  go (JSThrow _ js) = JSThrow ss js
  go (JSTypeOf _ js) = JSTypeOf ss js
  go (JSInstanceOf _ j1 j2) = JSInstanceOf ss j1 j2
  go (JSLabel _ name js) = JSLabel ss name js
  go (JSBreak _ s) = JSBreak ss s
  go (JSContinue _ s) = JSContinue ss s
  go (JSRaw _ s) = JSRaw ss s
  go (JSComment _ com j) = JSComment ss com j

getSourceSpan :: JS -> Maybe SourceSpan
getSourceSpan = go
  where
  go :: JS -> Maybe SourceSpan
  go (JSNumericLiteral ss _) = ss
  go (JSStringLiteral ss _) = ss
  go (JSBooleanLiteral ss _) = ss
  go (JSUnary ss _ _) = ss
  go (JSBinary ss _ _ _) = ss
  go (JSArrayLiteral ss _) = ss
  go (JSIndexer ss _ _) = ss
  go (JSObjectLiteral ss _) = ss
  go (JSAccessor ss _ _) = ss
  go (JSFunction ss _ _ _) = ss
  go (JSApp ss _ _) = ss
  go (JSVar ss _) = ss
  go (JSConditional ss _ _ _) = ss
  go (JSBlock ss _) = ss
  go (JSVariableIntroduction ss _ _) = ss
  go (JSAssignment ss _ _) = ss
  go (JSWhile ss _ _) = ss
  go (JSFor ss _ _ _ _) = ss
  go (JSForIn ss _ _ _) = ss
  go (JSIfElse ss _ _ _) = ss
  go (JSReturn ss _) = ss
  go (JSThrow ss _) = ss
  go (JSTypeOf ss _) = ss
  go (JSInstanceOf ss _ _) = ss
  go (JSLabel ss _ _) = ss
  go (JSBreak ss _) = ss
  go (JSContinue ss _) = ss
  go (JSRaw ss _) = ss
  go (JSComment ss _ _) = ss

--
-- Traversals
--

everywhereOnJS :: (JS -> JS) -> JS -> JS
everywhereOnJS f = go
  where
  go :: JS -> JS
  go (JSUnary ss op j) = f (JSUnary ss op (go j))
  go (JSBinary ss op j1 j2) = f (JSBinary ss op (go j1) (go j2))
  go (JSArrayLiteral ss js) = f (JSArrayLiteral ss (map go js))
  go (JSIndexer ss j1 j2) = f (JSIndexer ss (go j1) (go j2))
  go (JSObjectLiteral ss js) = f (JSObjectLiteral ss (map (fmap go) js))
  go (JSAccessor ss prop j) = f (JSAccessor ss prop (go j))
  go (JSFunction ss name args j) = f (JSFunction ss name args (go j))
  go (JSApp ss j js) = f (JSApp ss (go j) (map go js))
  go (JSConditional ss j1 j2 j3) = f (JSConditional ss (go j1) (go j2) (go j3))
  go (JSBlock ss js) = f (JSBlock ss (map go js))
  go (JSVariableIntroduction ss name j) = f (JSVariableIntroduction ss name (fmap go j))
  go (JSAssignment ss j1 j2) = f (JSAssignment ss (go j1) (go j2))
  go (JSWhile ss j1 j2) = f (JSWhile ss (go j1) (go j2))
  go (JSFor ss name j1 j2 j3) = f (JSFor ss name (go j1) (go j2) (go j3))
  go (JSForIn ss name j1 j2) = f (JSForIn ss name (go j1) (go j2))
  go (JSIfElse ss j1 j2 j3) = f (JSIfElse ss (go j1) (go j2) (fmap go j3))
  go (JSReturn ss js) = f (JSReturn ss (go js))
  go (JSThrow ss js) = f (JSThrow ss (go js))
  go (JSTypeOf ss js) = f (JSTypeOf ss (go js))
  go (JSLabel ss name js) = f (JSLabel ss name (go js))
  go (JSInstanceOf ss j1 j2) = f (JSInstanceOf ss (go j1) (go j2))
  go (JSComment ss com j) = f (JSComment ss com (go j))
  go other = f other

everywhereOnJSTopDown :: (JS -> JS) -> JS -> JS
everywhereOnJSTopDown f = runIdentity . everywhereOnJSTopDownM (Identity . f)

everywhereOnJSTopDownM :: (Monad m) => (JS -> m JS) -> JS -> m JS
everywhereOnJSTopDownM f = f >=> go
  where
  f' = f >=> go
  go (JSUnary ss op j) = JSUnary ss op <$> f' j
  go (JSBinary ss op j1 j2) = JSBinary ss op <$> f' j1 <*> f' j2
  go (JSArrayLiteral ss js) = JSArrayLiteral ss <$> traverse f' js
  go (JSIndexer ss j1 j2) = JSIndexer ss <$> f' j1 <*> f' j2
  go (JSObjectLiteral ss js) = JSObjectLiteral ss <$> traverse (sndM f') js
  go (JSAccessor ss prop j) = JSAccessor ss prop <$> f' j
  go (JSFunction ss name args j) = JSFunction ss name args <$> f' j
  go (JSApp ss j js) = JSApp ss <$> f' j <*> traverse f' js
  go (JSConditional ss j1 j2 j3) = JSConditional ss <$> f' j1 <*> f' j2 <*> f' j3
  go (JSBlock ss js) = JSBlock ss <$> traverse f' js
  go (JSVariableIntroduction ss name j) = JSVariableIntroduction ss name <$> traverse f' j
  go (JSAssignment ss j1 j2) = JSAssignment ss <$> f' j1 <*> f' j2
  go (JSWhile ss j1 j2) = JSWhile ss <$> f' j1 <*> f' j2
  go (JSFor ss name j1 j2 j3) = JSFor ss name <$> f' j1 <*> f' j2 <*> f' j3
  go (JSForIn ss name j1 j2) = JSForIn ss name <$> f' j1 <*> f' j2
  go (JSIfElse ss j1 j2 j3) = JSIfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (JSReturn ss j) = JSReturn ss <$> f' j
  go (JSThrow ss j) = JSThrow ss <$> f' j
  go (JSTypeOf ss j) = JSTypeOf ss <$> f' j
  go (JSLabel ss name j) = JSLabel ss name <$> f' j
  go (JSInstanceOf ss j1 j2) = JSInstanceOf ss <$> f' j1 <*> f' j2
  go (JSComment ss com j) = JSComment ss com <$> f' j
  go other = f other

everythingOnJS :: (r -> r -> r) -> (JS -> r) -> JS -> r
everythingOnJS (<>) f = go
  where
  go j@(JSUnary _ _ j1) = f j <> go j1
  go j@(JSBinary _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSArrayLiteral _ js) = foldl (<>) (f j) (map go js)
  go j@(JSIndexer _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSObjectLiteral _ js) = foldl (<>) (f j) (map (go . snd) js)
  go j@(JSAccessor _ _ j1) = f j <> go j1
  go j@(JSFunction _ _ _ j1) = f j <> go j1
  go j@(JSApp _ j1 js) = foldl (<>) (f j <> go j1) (map go js)
  go j@(JSConditional _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(JSBlock _ js) = foldl (<>) (f j) (map go js)
  go j@(JSVariableIntroduction _ _ (Just j1)) = f j <> go j1
  go j@(JSAssignment _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSWhile _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSFor _ _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(JSForIn _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSIfElse _ j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(JSIfElse _ j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(JSReturn _ j1) = f j <> go j1
  go j@(JSThrow _ j1) = f j <> go j1
  go j@(JSTypeOf _ j1) = f j <> go j1
  go j@(JSLabel _ _ j1) = f j <> go j1
  go j@(JSInstanceOf _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSComment _ _ j1) = f j <> go j1
  go other = f other
