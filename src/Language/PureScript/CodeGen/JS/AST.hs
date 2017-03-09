-- | Data types for the intermediate simplified-JavaScript AST
module Language.PureScript.CodeGen.JS.AST where

import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity(..), runIdentity)
import Data.Text (Text)

import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.Comments
import Language.PureScript.PSString (PSString)
import Language.PureScript.Traversals

-- | Built-in unary operators
data UnaryOperator
  = Negate
  | Not
  | BitwiseNot
  | Positive
  | JSNew
  deriving (Show, Eq)

-- | Built-in binary operators
data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ZeroFillShiftRight
  deriving (Show, Eq)

-- | Data type for simplified JavaScript expressions
data JS
  = JSNumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- ^ A numeric literal
  | JSStringLiteral (Maybe SourceSpan) PSString
  -- ^ A string literal
  | JSBooleanLiteral (Maybe SourceSpan) Bool
  -- ^ A boolean literal
  | JSUnary (Maybe SourceSpan) UnaryOperator JS
  -- ^ A unary operator application
  | JSBinary (Maybe SourceSpan) BinaryOperator JS JS
  -- ^ A binary operator application
  | JSArrayLiteral (Maybe SourceSpan) [JS]
  -- ^ An array literal
  | JSIndexer (Maybe SourceSpan) JS JS
  -- ^ An array indexer expression
  | JSObjectLiteral (Maybe SourceSpan) [(PSString, JS)]
  -- ^ An object literal
  | JSFunction (Maybe SourceSpan) (Maybe Text) [Text] JS
  -- ^ A function introduction (optional name, arguments, body)
  | JSApp (Maybe SourceSpan) JS [JS]
  -- ^ Function application
  | JSVar (Maybe SourceSpan) Text
  -- ^ Variable
  | JSBlock (Maybe SourceSpan) [JS]
  -- ^ A block of expressions in braces
  | JSVariableIntroduction (Maybe SourceSpan) Text (Maybe JS)
  -- ^ A variable introduction and optional initialization
  | JSAssignment (Maybe SourceSpan) JS JS
  -- ^ A variable assignment
  | JSWhile (Maybe SourceSpan) JS JS
  -- ^ While loop
  | JSFor (Maybe SourceSpan) Text JS JS JS
  -- ^ For loop
  | JSForIn (Maybe SourceSpan) Text JS JS
  -- ^ ForIn loop
  | JSIfElse (Maybe SourceSpan) JS JS (Maybe JS)
  -- ^ If-then-else statement
  | JSReturn (Maybe SourceSpan) JS
  -- ^ Return statement
  | JSReturnNoResult (Maybe SourceSpan)
  -- ^ Return statement with no return value
  | JSThrow (Maybe SourceSpan) JS
  -- ^ Throw statement
  | JSTypeOf (Maybe SourceSpan) JS
  -- ^ Type-Of operator
  | JSInstanceOf (Maybe SourceSpan) JS JS
  -- ^ instanceof check
  | JSComment (Maybe SourceSpan) [Comment] JS
  -- ^ Commented JavaScript
  deriving (Show, Eq)

withSourceSpan :: SourceSpan -> JS -> JS
withSourceSpan withSpan = go where
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
  go (JSFunction _ name args j) = JSFunction ss name args j
  go (JSApp _ j js) = JSApp ss j js
  go (JSVar _ s) = JSVar ss s
  go (JSBlock _ js) = JSBlock ss js
  go (JSVariableIntroduction _ name j) = JSVariableIntroduction ss name j
  go (JSAssignment _ j1 j2) = JSAssignment ss j1 j2
  go (JSWhile _ j1 j2) = JSWhile ss j1 j2
  go (JSFor _ name j1 j2 j3) = JSFor ss name j1 j2 j3
  go (JSForIn _ name j1 j2) = JSForIn ss name j1 j2
  go (JSIfElse _ j1 j2 j3) = JSIfElse ss j1 j2 j3
  go (JSReturn _ js) = JSReturn ss js
  go (JSReturnNoResult _) = JSReturnNoResult ss
  go (JSThrow _ js) = JSThrow ss js
  go (JSTypeOf _ js) = JSTypeOf ss js
  go (JSInstanceOf _ j1 j2) = JSInstanceOf ss j1 j2
  go (JSComment _ com j) = JSComment ss com j

getSourceSpan :: JS -> Maybe SourceSpan
getSourceSpan = go where
  go :: JS -> Maybe SourceSpan
  go (JSNumericLiteral ss _) = ss
  go (JSStringLiteral ss _) = ss
  go (JSBooleanLiteral ss _) = ss
  go (JSUnary ss _ _) = ss
  go (JSBinary ss _ _ _) = ss
  go (JSArrayLiteral ss _) = ss
  go (JSIndexer ss _ _) = ss
  go (JSObjectLiteral ss _) = ss
  go (JSFunction ss _ _ _) = ss
  go (JSApp ss _ _) = ss
  go (JSVar ss _) = ss
  go (JSBlock ss _) = ss
  go (JSVariableIntroduction ss _ _) = ss
  go (JSAssignment ss _ _) = ss
  go (JSWhile ss _ _) = ss
  go (JSFor ss _ _ _ _) = ss
  go (JSForIn ss _ _ _) = ss
  go (JSIfElse ss _ _ _) = ss
  go (JSReturn ss _) = ss
  go (JSReturnNoResult ss) = ss
  go (JSThrow ss _) = ss
  go (JSTypeOf ss _) = ss
  go (JSInstanceOf ss _ _) = ss
  go (JSComment ss _ _) = ss

everywhereOnJS :: (JS -> JS) -> JS -> JS
everywhereOnJS f = go where
  go :: JS -> JS
  go (JSUnary ss op j) = f (JSUnary ss op (go j))
  go (JSBinary ss op j1 j2) = f (JSBinary ss op (go j1) (go j2))
  go (JSArrayLiteral ss js) = f (JSArrayLiteral ss (map go js))
  go (JSIndexer ss j1 j2) = f (JSIndexer ss (go j1) (go j2))
  go (JSObjectLiteral ss js) = f (JSObjectLiteral ss (map (fmap go) js))
  go (JSFunction ss name args j) = f (JSFunction ss name args (go j))
  go (JSApp ss j js) = f (JSApp ss (go j) (map go js))
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
  go (JSInstanceOf ss j1 j2) = f (JSInstanceOf ss (go j1) (go j2))
  go (JSComment ss com j) = f (JSComment ss com (go j))
  go other = f other

everywhereOnJSTopDown :: (JS -> JS) -> JS -> JS
everywhereOnJSTopDown f = runIdentity . everywhereOnJSTopDownM (Identity . f)

everywhereOnJSTopDownM :: (Monad m) => (JS -> m JS) -> JS -> m JS
everywhereOnJSTopDownM f = f >=> go where
  f' = f >=> go
  go (JSUnary ss op j) = JSUnary ss op <$> f' j
  go (JSBinary ss op j1 j2) = JSBinary ss op <$> f' j1 <*> f' j2
  go (JSArrayLiteral ss js) = JSArrayLiteral ss <$> traverse f' js
  go (JSIndexer ss j1 j2) = JSIndexer ss <$> f' j1 <*> f' j2
  go (JSObjectLiteral ss js) = JSObjectLiteral ss <$> traverse (sndM f') js
  go (JSFunction ss name args j) = JSFunction ss name args <$> f' j
  go (JSApp ss j js) = JSApp ss <$> f' j <*> traverse f' js
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
  go (JSInstanceOf ss j1 j2) = JSInstanceOf ss <$> f' j1 <*> f' j2
  go (JSComment ss com j) = JSComment ss com <$> f' j
  go other = f other

everythingOnJS :: (r -> r -> r) -> (JS -> r) -> JS -> r
everythingOnJS (<>) f = go where
  go j@(JSUnary _ _ j1) = f j <> go j1
  go j@(JSBinary _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSArrayLiteral _ js) = foldl (<>) (f j) (map go js)
  go j@(JSIndexer _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSObjectLiteral _ js) = foldl (<>) (f j) (map (go . snd) js)
  go j@(JSFunction _ _ _ j1) = f j <> go j1
  go j@(JSApp _ j1 js) = foldl (<>) (f j <> go j1) (map go js)
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
  go j@(JSInstanceOf _ j1 j2) = f j <> go j1 <> go j2
  go j@(JSComment _ _ j1) = f j <> go j1
  go other = f other
