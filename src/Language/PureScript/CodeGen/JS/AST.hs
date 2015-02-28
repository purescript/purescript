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

module Language.PureScript.CodeGen.JS.AST where

import Data.Data

import Language.PureScript.CoreImp.Operators
import Language.PureScript.Comments

-- |
-- Built-in unary operators
--
data JSUnaryOp
  -- |
  -- Numeric negation
  --
  = JSNegate
  -- |
  -- Boolean negation
  --
  | JSNot
  -- |
  -- Bitwise negation
  --
  | JSBitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | JSPositive
  -- |
  -- Constructor
  --
  | JSNew deriving (Show, Eq, Data, Typeable)

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
  | JSUnary JSUnaryOp JS
  -- |
  -- A binary operator application
  --
  | JSBinary BinaryOp JS JS
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
  | JSComment [Comment] JS deriving (Show, Eq, Data, Typeable)

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
everywhereOnJSTopDown f = go . f
  where
  go :: JS -> JS
  go (JSUnary op j) = JSUnary op (go (f j))
  go (JSBinary op j1 j2) = JSBinary op (go (f j1)) (go (f j2))
  go (JSArrayLiteral js) = JSArrayLiteral (map (go . f) js)
  go (JSIndexer j1 j2) = JSIndexer (go (f j1)) (go (f j2))
  go (JSObjectLiteral js) = JSObjectLiteral (map (fmap (go . f)) js)
  go (JSAccessor prop j) = JSAccessor prop (go (f j))
  go (JSFunction name args j) = JSFunction name args (go (f j))
  go (JSApp j js) = JSApp (go (f j)) (map (go . f) js)
  go (JSConditional j1 j2 j3) = JSConditional (go (f j1)) (go (f j2)) (go (f j3))
  go (JSBlock js) = JSBlock (map (go . f) js)
  go (JSVariableIntroduction name j) = JSVariableIntroduction name (fmap (go . f) j)
  go (JSAssignment j1 j2) = JSAssignment (go (f j1)) (go (f j2))
  go (JSWhile j1 j2) = JSWhile (go (f j1)) (go (f j2))
  go (JSFor name j1 j2 j3) = JSFor name (go (f j1)) (go (f j2)) (go (f j3))
  go (JSForIn name j1 j2) = JSForIn name (go (f j1)) (go (f j2))
  go (JSIfElse j1 j2 j3) = JSIfElse (go (f j1)) (go (f j2)) (fmap (go . f) j3)
  go (JSReturn j) = JSReturn (go (f j))
  go (JSThrow j) = JSThrow (go (f j))
  go (JSTypeOf j) = JSTypeOf (go (f j))
  go (JSLabel name j) = JSLabel name (go (f j))
  go (JSInstanceOf j1 j2) = JSInstanceOf (go (f j1)) (go (f j2))
  go (JSComment com j) = JSComment com (go (f j))
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
