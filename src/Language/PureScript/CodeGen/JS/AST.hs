-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS.AST
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Data types for the intermediate simplified-Javascript AST
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.JS.AST where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative, (<$>), (<*>))
#endif
import Control.Monad.Identity
import Data.Data
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif

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
  | JSNew deriving (Show, Read, Eq, Data, Typeable)

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
  | ZeroFillShiftRight deriving (Show, Read, Eq, Data, Typeable)

-- |
-- Data type for simplified Javascript expressions
--
data JS
  -- |
  -- A numeric literal
  --
  = JSNumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | JSStringLiteral String
  -- |
  -- A boolean literal
  --
  | JSBooleanLiteral Bool
  -- |
  -- A unary operator application
  --
  | JSUnary UnaryOperator JS
  -- |
  -- A binary operator application
  --
  | JSBinary BinaryOperator JS JS
  -- |
  -- An array literal
  --
  | JSArrayLiteral [JS]
  -- |
  -- An array indexer expression
  --
  | JSIndexer JS JS
  -- |
  -- An object literal
  --
  | JSObjectLiteral [(String, JS)]
  -- |
  -- An object property accessor expression
  --
  | JSAccessor String JS
  -- |
  -- A function introduction (optional name, arguments, body)
  --
  | JSFunction (Maybe String) [String] JS
  -- |
  -- Function application
  --
  | JSApp JS [JS]
  -- |
  -- Variable
  --
  | JSVar String
  -- |
  -- Conditional expression
  --
  | JSConditional JS JS JS
  -- |
  -- A block of expressions in braces
  --
  | JSBlock [JS]
  -- |
  -- A variable introduction and optional initialization
  --
  | JSVariableIntroduction String (Maybe JS)
  -- |
  -- A variable assignment
  --
  | JSAssignment JS JS
  -- |
  -- While loop
  --
  | JSWhile JS JS
  -- |
  -- For loop
  --
  | JSFor String JS JS JS
  -- |
  -- ForIn loop
  --
  | JSForIn String JS JS
  -- |
  -- If-then-else statement
  --
  | JSIfElse JS JS (Maybe JS)
  -- |
  -- Return statement
  --
  | JSReturn JS
  -- |
  -- Throw statement
  --
  | JSThrow JS
  -- |
  -- Type-Of operator
  --
  | JSTypeOf JS
  -- |
  -- InstanceOf test
  --
  | JSInstanceOf JS JS
  -- |
  -- Labelled statement
  --
  | JSLabel String JS
  -- |
  -- Break statement
  --
  | JSBreak String
  -- |
  -- Continue statement
  --
  | JSContinue String
  -- |
  -- Raw Javascript (generated when parsing fails for an inline foreign import declaration)
  --
  | JSRaw String
  -- |
  -- Commented Javascript
  --
  | JSComment [Comment] JS deriving (Show, Read, Eq, Data, Typeable)

--
-- Traversals
--

everywhereOnJS :: (JS -> JS) -> JS -> JS
everywhereOnJS f = go
  where
  go :: JS -> JS
  go (JSUnary op j) = f (JSUnary op (go j))
  go (JSBinary op j1 j2) = f (JSBinary op (go j1) (go j2))
  go (JSArrayLiteral js) = f (JSArrayLiteral (map go js))
  go (JSIndexer j1 j2) = f (JSIndexer (go j1) (go j2))
  go (JSObjectLiteral js) = f (JSObjectLiteral (map (fmap go) js))
  go (JSAccessor prop j) = f (JSAccessor prop (go j))
  go (JSFunction name args j) = f (JSFunction name args (go j))
  go (JSApp j js) = f (JSApp (go j) (map go js))
  go (JSConditional j1 j2 j3) = f (JSConditional (go j1) (go j2) (go j3))
  go (JSBlock js) = f (JSBlock (map go js))
  go (JSVariableIntroduction name j) = f (JSVariableIntroduction name (fmap go j))
  go (JSAssignment j1 j2) = f (JSAssignment (go j1) (go j2))
  go (JSWhile j1 j2) = f (JSWhile (go j1) (go j2))
  go (JSFor name j1 j2 j3) = f (JSFor name (go j1) (go j2) (go j3))
  go (JSForIn name j1 j2) = f (JSForIn name (go j1) (go j2))
  go (JSIfElse j1 j2 j3) = f (JSIfElse (go j1) (go j2) (fmap go j3))
  go (JSReturn js) = f (JSReturn (go js))
  go (JSThrow js) = f (JSThrow (go js))
  go (JSTypeOf js) = f (JSTypeOf (go js))
  go (JSLabel name js) = f (JSLabel name (go js))
  go (JSInstanceOf j1 j2) = f (JSInstanceOf (go j1) (go j2))
  go (JSComment com j) = f (JSComment com (go j))
  go other = f other

everywhereOnJSTopDown :: (JS -> JS) -> JS -> JS
everywhereOnJSTopDown f = runIdentity . everywhereOnJSTopDownM (Identity . f)

everywhereOnJSTopDownM :: (Applicative m, Monad m) => (JS -> m JS) -> JS -> m JS
everywhereOnJSTopDownM f = f >=> go
  where
  f' = f >=> go
  go (JSUnary op j) = JSUnary op <$> f' j
  go (JSBinary op j1 j2) = JSBinary op <$> f' j1 <*> f' j2
  go (JSArrayLiteral js) = JSArrayLiteral <$> traverse f' js
  go (JSIndexer j1 j2) = JSIndexer <$> f' j1 <*> f' j2
  go (JSObjectLiteral js) = JSObjectLiteral <$> traverse (sndM f') js
  go (JSAccessor prop j) = JSAccessor prop <$> f' j
  go (JSFunction name args j) = JSFunction name args <$> f' j
  go (JSApp j js) = JSApp <$> f' j <*> traverse f' js
  go (JSConditional j1 j2 j3) = JSConditional <$> f' j1 <*> f' j2 <*> f' j3
  go (JSBlock js) = JSBlock <$> traverse f' js
  go (JSVariableIntroduction name j) = JSVariableIntroduction name <$> traverse f' j
  go (JSAssignment j1 j2) = JSAssignment <$> f' j1 <*> f' j2
  go (JSWhile j1 j2) = JSWhile <$> f' j1 <*> f' j2
  go (JSFor name j1 j2 j3) = JSFor name <$> f' j1 <*> f' j2 <*> f' j3
  go (JSForIn name j1 j2) = JSForIn name <$> f' j1 <*> f' j2
  go (JSIfElse j1 j2 j3) = JSIfElse <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (JSReturn j) = JSReturn <$> f' j
  go (JSThrow j) = JSThrow <$> f' j
  go (JSTypeOf j) = JSTypeOf <$> f' j
  go (JSLabel name j) = JSLabel name <$> f' j
  go (JSInstanceOf j1 j2) = JSInstanceOf <$> f' j1 <*> f' j2
  go (JSComment com j) = JSComment com <$> f' j
  go other = f other

everythingOnJS :: (r -> r -> r) -> (JS -> r) -> JS -> r
everythingOnJS (<>) f = go
  where
  go j@(JSUnary _ j1) = f j <> go j1
  go j@(JSBinary _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSArrayLiteral js) = foldl (<>) (f j) (map go js)
  go j@(JSIndexer j1 j2) = f j <> go j1 <> go j2
  go j@(JSObjectLiteral js) = foldl (<>) (f j) (map (go . snd) js)
  go j@(JSAccessor _ j1) = f j <> go j1
  go j@(JSFunction _ _ j1) = f j <> go j1
  go j@(JSApp j1 js) = foldl (<>) (f j <> go j1) (map go js)
  go j@(JSConditional j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(JSBlock js) = foldl (<>) (f j) (map go js)
  go j@(JSVariableIntroduction _ (Just j1)) = f j <> go j1
  go j@(JSAssignment j1 j2) = f j <> go j1 <> go j2
  go j@(JSWhile j1 j2) = f j <> go j1 <> go j2
  go j@(JSFor _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(JSForIn _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSIfElse j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(JSIfElse j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(JSReturn j1) = f j <> go j1
  go j@(JSThrow j1) = f j <> go j1
  go j@(JSTypeOf j1) = f j <> go j1
  go j@(JSLabel _ j1) = f j <> go j1
  go j@(JSInstanceOf j1 j2) = f j <> go j1 <> go j2
  go j@(JSComment _ j1) = f j <> go j1
  go other = f other
