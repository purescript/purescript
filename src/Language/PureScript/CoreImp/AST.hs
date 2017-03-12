-- | Data types for the imperative core AST
module Language.PureScript.CoreImp.AST where

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
  | New
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
data AST
  = NumericLiteral (Maybe SourceSpan) (Either Integer Double)
  -- ^ A numeric literal
  | StringLiteral (Maybe SourceSpan) PSString
  -- ^ A string literal
  | BooleanLiteral (Maybe SourceSpan) Bool
  -- ^ A boolean literal
  | Unary (Maybe SourceSpan) UnaryOperator AST
  -- ^ A unary operator application
  | Binary (Maybe SourceSpan) BinaryOperator AST AST
  -- ^ A binary operator application
  | ArrayLiteral (Maybe SourceSpan) [AST]
  -- ^ An array literal
  | Indexer (Maybe SourceSpan) AST AST
  -- ^ An array indexer expression
  | ObjectLiteral (Maybe SourceSpan) [(PSString, AST)]
  -- ^ An object literal
  | Function (Maybe SourceSpan) (Maybe Text) [Text] AST
  -- ^ A function introduction (optional name, arguments, body)
  | App (Maybe SourceSpan) AST [AST]
  -- ^ Function application
  | Var (Maybe SourceSpan) Text
  -- ^ Variable
  | Block (Maybe SourceSpan) [AST]
  -- ^ A block of expressions in braces
  | VariableIntroduction (Maybe SourceSpan) Text (Maybe AST)
  -- ^ A variable introduction and optional initialization
  | Assignment (Maybe SourceSpan) AST AST
  -- ^ A variable assignment
  | While (Maybe SourceSpan) AST AST
  -- ^ While loop
  | For (Maybe SourceSpan) Text AST AST AST
  -- ^ For loop
  | ForIn (Maybe SourceSpan) Text AST AST
  -- ^ ForIn loop
  | IfElse (Maybe SourceSpan) AST AST (Maybe AST)
  -- ^ If-then-else statement
  | Return (Maybe SourceSpan) AST
  -- ^ Return statement
  | ReturnNoResult (Maybe SourceSpan)
  -- ^ Return statement with no return value
  | Throw (Maybe SourceSpan) AST
  -- ^ Throw statement
  | InstanceOf (Maybe SourceSpan) AST AST
  -- ^ instanceof check
  | Comment (Maybe SourceSpan) [Comment] AST
  -- ^ Commented JavaScript
  deriving (Show, Eq)

withSourceSpan :: SourceSpan -> AST -> AST
withSourceSpan withSpan = go where
  ss :: Maybe SourceSpan
  ss = Just withSpan

  go :: AST -> AST
  go (NumericLiteral _ n) = NumericLiteral ss n
  go (StringLiteral _ s) = StringLiteral ss s
  go (BooleanLiteral _ b) = BooleanLiteral ss b
  go (Unary _ op j) = Unary ss op j
  go (Binary _ op j1 j2) = Binary ss op j1 j2
  go (ArrayLiteral _ js) = ArrayLiteral ss js
  go (Indexer _ j1 j2) = Indexer ss j1 j2
  go (ObjectLiteral _ js) = ObjectLiteral ss js
  go (Function _ name args j) = Function ss name args j
  go (App _ j js) = App ss j js
  go (Var _ s) = Var ss s
  go (Block _ js) = Block ss js
  go (VariableIntroduction _ name j) = VariableIntroduction ss name j
  go (Assignment _ j1 j2) = Assignment ss j1 j2
  go (While _ j1 j2) = While ss j1 j2
  go (For _ name j1 j2 j3) = For ss name j1 j2 j3
  go (ForIn _ name j1 j2) = ForIn ss name j1 j2
  go (IfElse _ j1 j2 j3) = IfElse ss j1 j2 j3
  go (Return _ js) = Return ss js
  go (ReturnNoResult _) = ReturnNoResult ss
  go (Throw _ js) = Throw ss js
  go (InstanceOf _ j1 j2) = InstanceOf ss j1 j2
  go (Comment _ com j) = Comment ss com j

getSourceSpan :: AST -> Maybe SourceSpan
getSourceSpan = go where
  go :: AST -> Maybe SourceSpan
  go (NumericLiteral ss _) = ss
  go (StringLiteral ss _) = ss
  go (BooleanLiteral ss _) = ss
  go (Unary ss _ _) = ss
  go (Binary ss _ _ _) = ss
  go (ArrayLiteral ss _) = ss
  go (Indexer ss _ _) = ss
  go (ObjectLiteral ss _) = ss
  go (Function ss _ _ _) = ss
  go (App ss _ _) = ss
  go (Var ss _) = ss
  go (Block ss _) = ss
  go (VariableIntroduction ss _ _) = ss
  go (Assignment ss _ _) = ss
  go (While ss _ _) = ss
  go (For ss _ _ _ _) = ss
  go (ForIn ss _ _ _) = ss
  go (IfElse ss _ _ _) = ss
  go (Return ss _) = ss
  go (ReturnNoResult ss) = ss
  go (Throw ss _) = ss
  go (InstanceOf ss _ _) = ss
  go (Comment ss _ _) = ss

everywhere :: (AST -> AST) -> AST -> AST
everywhere f = go where
  go :: AST -> AST
  go (Unary ss op j) = f (Unary ss op (go j))
  go (Binary ss op j1 j2) = f (Binary ss op (go j1) (go j2))
  go (ArrayLiteral ss js) = f (ArrayLiteral ss (map go js))
  go (Indexer ss j1 j2) = f (Indexer ss (go j1) (go j2))
  go (ObjectLiteral ss js) = f (ObjectLiteral ss (map (fmap go) js))
  go (Function ss name args j) = f (Function ss name args (go j))
  go (App ss j js) = f (App ss (go j) (map go js))
  go (Block ss js) = f (Block ss (map go js))
  go (VariableIntroduction ss name j) = f (VariableIntroduction ss name (fmap go j))
  go (Assignment ss j1 j2) = f (Assignment ss (go j1) (go j2))
  go (While ss j1 j2) = f (While ss (go j1) (go j2))
  go (For ss name j1 j2 j3) = f (For ss name (go j1) (go j2) (go j3))
  go (ForIn ss name j1 j2) = f (ForIn ss name (go j1) (go j2))
  go (IfElse ss j1 j2 j3) = f (IfElse ss (go j1) (go j2) (fmap go j3))
  go (Return ss js) = f (Return ss (go js))
  go (Throw ss js) = f (Throw ss (go js))
  go (InstanceOf ss j1 j2) = f (InstanceOf ss (go j1) (go j2))
  go (Comment ss com j) = f (Comment ss com (go j))
  go other = f other

everywhereTopDown :: (AST -> AST) -> AST -> AST
everywhereTopDown f = runIdentity . everywhereTopDownM (Identity . f)

everywhereTopDownM :: (Monad m) => (AST -> m AST) -> AST -> m AST
everywhereTopDownM f = f >=> go where
  f' = f >=> go
  go (Unary ss op j) = Unary ss op <$> f' j
  go (Binary ss op j1 j2) = Binary ss op <$> f' j1 <*> f' j2
  go (ArrayLiteral ss js) = ArrayLiteral ss <$> traverse f' js
  go (Indexer ss j1 j2) = Indexer ss <$> f' j1 <*> f' j2
  go (ObjectLiteral ss js) = ObjectLiteral ss <$> traverse (sndM f') js
  go (Function ss name args j) = Function ss name args <$> f' j
  go (App ss j js) = App ss <$> f' j <*> traverse f' js
  go (Block ss js) = Block ss <$> traverse f' js
  go (VariableIntroduction ss name j) = VariableIntroduction ss name <$> traverse f' j
  go (Assignment ss j1 j2) = Assignment ss <$> f' j1 <*> f' j2
  go (While ss j1 j2) = While ss <$> f' j1 <*> f' j2
  go (For ss name j1 j2 j3) = For ss name <$> f' j1 <*> f' j2 <*> f' j3
  go (ForIn ss name j1 j2) = ForIn ss name <$> f' j1 <*> f' j2
  go (IfElse ss j1 j2 j3) = IfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (Return ss j) = Return ss <$> f' j
  go (Throw ss j) = Throw ss <$> f' j
  go (InstanceOf ss j1 j2) = InstanceOf ss <$> f' j1 <*> f' j2
  go (Comment ss com j) = Comment ss com <$> f' j
  go other = f other

everything :: (r -> r -> r) -> (AST -> r) -> AST -> r
everything (<>) f = go where
  go j@(Unary _ _ j1) = f j <> go j1
  go j@(Binary _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(ArrayLiteral _ js) = foldl (<>) (f j) (map go js)
  go j@(Indexer _ j1 j2) = f j <> go j1 <> go j2
  go j@(ObjectLiteral _ js) = foldl (<>) (f j) (map (go . snd) js)
  go j@(Function _ _ _ j1) = f j <> go j1
  go j@(App _ j1 js) = foldl (<>) (f j <> go j1) (map go js)
  go j@(Block _ js) = foldl (<>) (f j) (map go js)
  go j@(VariableIntroduction _ _ (Just j1)) = f j <> go j1
  go j@(Assignment _ j1 j2) = f j <> go j1 <> go j2
  go j@(While _ j1 j2) = f j <> go j1 <> go j2
  go j@(For _ _ j1 j2 j3) = f j <> go j1 <> go j2 <> go j3
  go j@(ForIn _ _ j1 j2) = f j <> go j1 <> go j2
  go j@(IfElse _ j1 j2 Nothing) = f j <> go j1 <> go j2
  go j@(IfElse _ j1 j2 (Just j3)) = f j <> go j1 <> go j2 <> go j3
  go j@(Return _ j1) = f j <> go j1
  go j@(Throw _ j1) = f j <> go j1
  go j@(InstanceOf _ j1 j2) = f j <> go j1 <> go j2
  go j@(Comment _ _ j1) = f j <> go j1
  go other = f other
