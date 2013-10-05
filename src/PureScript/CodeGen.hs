-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module PureScript.CodeGen (
    declToJs,
    externToJs
) where

import Data.Maybe (fromMaybe)
import Data.List (nub, intersperse, intercalate)
import Data.Function (fix)
import Control.Monad.State
import Control.Applicative (Applicative(..), Alternative(..))
import qualified Control.Category as C
import Control.Category ((>>>))
import qualified Control.Arrow as A
import Control.Arrow ((***))

import PureScript.Values
import PureScript.Types
import PureScript.Declarations

-- First Order Patterns

newtype Pattern a b = Pattern { runPattern :: A.Kleisli Maybe a b } deriving (C.Category, A.Arrow)

pattern :: Pattern a b -> a -> Maybe b
pattern = A.runKleisli . runPattern

instance Functor (Pattern a) where
  fmap f p = Pattern $ A.Kleisli $ fmap f . pattern p

instance Applicative (Pattern a) where
  pure = Pattern . A.Kleisli . const . pure
  f <*> x = Pattern . A.Kleisli $ \a -> pattern f a <*> pattern x a

instance Alternative (Pattern a) where
  empty = Pattern $ A.Kleisli $ \a -> empty
  (Pattern (A.Kleisli p)) <|> (Pattern (A.Kleisli q)) = Pattern $ A.Kleisli $ \a -> p a <|> q a

parens :: String -> String
parens s = ('(':s) ++ ")"

chainl :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainl split f p = fix $ \c -> (split >>> (c *** p) >>> A.arr (uncurry f)) <|> p

chainr :: Pattern a (a, a) -> (r -> r -> r) -> Pattern a r -> Pattern a r
chainr split f p = fix $ \c -> (split >>> (p *** c) >>> A.arr (uncurry f)) <|> p

wrap :: Pattern a (s, a) -> (s -> r -> r) -> Pattern a r -> Pattern a r
wrap split f p = fix $ \c -> (split >>> (C.id *** c) >>> A.arr (uncurry f)) <|> p

split :: Pattern a (s, t) -> (s -> t -> r) -> Pattern a r -> Pattern a r
split s f p = (s >>> A.arr (uncurry f)) <|> p

data OperatorTable a r = OperatorTable { runOperatorTable :: [Operator a r] }

data Operator a r where
  AssocL :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  AssocR :: Pattern a (a, a) -> (r -> r -> r) -> Operator a r
  Wrap   :: Pattern a (s, a) -> (s -> r -> r) -> Operator a r
  Split  :: Pattern a (s, t) -> (s -> t -> r) -> Operator a r

buildPrettyPrinter :: OperatorTable a r -> Pattern a r -> Pattern a r
buildPrettyPrinter table p = foldl (\p' op ->
  case op of
    AssocL pat g -> chainl pat g p'
    AssocR pat g -> chainr pat g p'
    Wrap pat g -> wrap pat g p'
    Split pat g -> split pat g p'
  ) p $ runOperatorTable table

-- Variable Name Generation

newtype Gen a = Gen { unGen :: State Int a } deriving (Functor, Monad, MonadState Int)

runGen :: Gen a -> a
runGen = flip evalState 0 . unGen

fresh :: Gen String
fresh = do
  n <- get
  modify (+ 1)
  return $ "_" ++ show n

-- Pretty Printing

declToJs :: Declaration -> Maybe String
declToJs (ValueDeclaration name val) = Just $ "var " ++ name ++ " = " ++ valueToJs val ++ ";"
declToJs (DataDeclaration dcs@(DataConstructors { dataConstructors = ctors })) =
  Just $ flip concatMap ctors $ \(ctor, maybeTy) ->
    case maybeTy of
      Nothing -> "var " ++ ctor ++ " =  { ctor: '" ++ ctor ++ "' };"
      Just _ -> "var " ++ ctor ++ " = function (value) { return { ctor: '" ++ ctor ++ "', value: value }; };"
declToJs _ = Nothing

literals :: Pattern Value String
literals = Pattern $ A.Kleisli match
  where
  match (NumericLiteral n) = Just $ either show show n
  match (StringLiteral s) = Just $ show s
  match (BooleanLiteral True) = Just "true"
  match (BooleanLiteral False) = Just "false"
  match (ArrayLiteral xs) = Just $ "[" ++ intercalate "," (map valueToJs xs) ++ "]"
  match (ObjectLiteral ps) = Just $ "{" ++ intercalate "," (map objectPropertyToJs ps) ++ "}"
  match (Constructor name) = Just name
  match (Block sts) = Just $ "(function () {" ++ intercalate ";" (map statementToJs sts) ++ "})()"
  match (Case value binders) = Just $ "(" ++ runGen (bindersToJs binders) ++  ")(" ++ valueToJs value ++ ")"
    where
    bindersToJs ::  [(Binder, Value)] -> Gen String
    bindersToJs binders = do
      valName <- fresh
      jss <- flip mapM binders $ \(binder, result) -> do
         let js = valueToJs result
         binderToJs valName ("return " ++ js ++ ";") binder
      return $ "function (" ++ valName ++ ") {" ++ concat jss ++ "throw \"Failed pattern match\"; }"
  match (Var ident) = Just ident
  match _ = Nothing

accessor :: Pattern Value (String, Value)
accessor = Pattern $ A.Kleisli match
  where
  match (Accessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern Value (String, Value)
indexer = Pattern $ A.Kleisli match
  where
  match (Indexer index val) = Just (valueToJs index, val)
  match _ = Nothing

app :: Pattern Value (String, Value)
app = Pattern $ A.Kleisli match
  where
  match (App val args) = Just (intercalate "," (map valueToJs args), val)
  match _ = Nothing

lam :: Pattern Value ([String], Value)
lam = Pattern $ A.Kleisli match
  where
  match (Abs args val) = Just (args, val)
  match _ = Nothing

unary :: UnaryOperator -> String -> Operator Value String
unary op str = Wrap pattern (++)
  where
  pattern :: Pattern Value (String, Value)
  pattern = Pattern $ A.Kleisli match
    where
    match (Unary op' val) | op' == op = Just (str, val)
    match _ = Nothing

binary :: BinaryOperator -> String -> Operator Value String
binary op str = AssocR pattern (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  pattern :: Pattern Value (Value, Value)
  pattern = Pattern $ A.Kleisli match
    where
    match (Binary op' v1 v2) | op' == op = Just (v1, v2)
    match _ = Nothing

valueToJs :: Value -> String
valueToJs = fromMaybe (error "Incomplete pattern") . pattern matchValue
  where
  matchValue :: Pattern Value String
  matchValue = buildPrettyPrinter operators (literals <|> fmap parens matchValue)
  operators :: OperatorTable Value String
  operators =
    OperatorTable $ [ Wrap accessor $ \prop val -> val ++ "." ++ prop
                    , Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]"
                    , Wrap app $ \args val -> val ++ "(" ++ args ++ ")"
                    , Split lam $ \args val -> "function (" ++ intercalate "," args ++ ") { return " ++ valueToJs val ++ "; }"
                    , binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Negate               "-"
                    , binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%"
                    , binary    Concat               "++"
                    , binary    Add                  "+"
                    , binary    Subtract             "-"
                    , binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>"
                    , binary    EqualTo              "==="
                    , binary    NotEqualTo           "!=="
                    , binary    BitwiseAnd           "&"
                    , binary    BitwiseXor           "^"
                    , binary    BitwiseOr            "|"
                    , binary    And                  "&&"
                    , binary    Or                   "||"
                    ]

unaryOperatorString :: UnaryOperator -> String
unaryOperatorString Negate = "-"
unaryOperatorString Not = "!"
unaryOperatorString BitwiseNot = "~"

binaryOperatorString :: BinaryOperator -> String
binaryOperatorString Add = "+"
binaryOperatorString Subtract = "-"
binaryOperatorString Multiply = "*"
binaryOperatorString Divide = "/"
binaryOperatorString Modulus = "%"
binaryOperatorString LessThan = "<"
binaryOperatorString LessThanOrEqualTo = "<="
binaryOperatorString GreaterThan = ">"
binaryOperatorString GreaterThanOrEqualTo = ">="
binaryOperatorString BitwiseAnd = "&"
binaryOperatorString BitwiseOr = "|"
binaryOperatorString BitwiseXor = "^"
binaryOperatorString ShiftLeft = "<<"
binaryOperatorString ShiftRight = ">>"
binaryOperatorString ZeroFillShiftRight = ">>>"
binaryOperatorString EqualTo = "==="
binaryOperatorString NotEqualTo = "!=="
binaryOperatorString And = "&&"
binaryOperatorString Or = "||"
binaryOperatorString Concat = "+"

binderToJs :: String -> String -> Binder -> Gen String
binderToJs varName done (StringBinder str) =
  return $ "if (" ++ varName ++ " === \"" ++ str ++ "\") {" ++ done ++ " }"
binderToJs varName done (NumberBinder num) =
  return $ "if (" ++ varName ++ " === \"" ++ either show show num ++ "\") {" ++ done ++ " }"
binderToJs varName done (BooleanBinder True) =
  return $ "if (" ++ varName ++ ") {" ++ done ++ " }"
binderToJs varName done (BooleanBinder False) =
  return $ "if (!" ++ varName ++ ") {" ++ done ++ " }"
binderToJs varName done (VarBinder s) =
  return $ "var " ++ s ++ " = " ++ varName ++ "; " ++ done
binderToJs varName done (NullaryBinder ctor) =
  return $ "if (" ++ varName ++ ".ctor === \"" ++ ctor ++ "\") { " ++ done ++ " }"
binderToJs varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs value done b
  return $ "if (" ++ varName ++ ".ctor === \"" ++ ctor ++ "\") { " ++ "var " ++ value ++ " = " ++ varName ++ ".value; " ++ js ++ " }"
binderToJs varName done (ObjectBinder bs) = go done bs
  where
  go done [] = return done
  go done ((prop, binder):bs) = do
    propVar <- fresh
    done' <- go done bs
    js <- binderToJs propVar done' binder
    return $ "var " ++ propVar ++ " = " ++ varName ++ "." ++ prop ++ ";" ++ js
binderToJs done varName (ArrayBinder bs rest) = do
  js <- go done rest 0 bs
  return $ "if (" ++ varName ++ ".length " ++ cmp ++ " " ++ show (length bs) ++ ") { " ++ js ++ " }"
  where
  cmp = maybe "===" (const ">=") rest
  go done Nothing _ [] = return done
  go done (Just binder) index [] = do
    restVar <- fresh
    js <- binderToJs restVar done binder
    return $ "var " ++ restVar ++ " = " ++ varName ++ ".slice(" ++ show index ++ "); " ++ js
  go done rest index (binder:bs) = do
    elVar <- fresh
    done' <- go done rest (index + 1) bs
    js <- binderToJs elVar done' binder
    return $ "var " ++ elVar ++ " = " ++ varName ++ "[" ++ show index ++ "]; " ++ js

objectPropertyToJs :: (String, Value) -> String
objectPropertyToJs (key, value) = key ++ ":" ++ valueToJs value

statementToJs :: Statement -> String
statementToJs (VariableIntroduction name value) = "var " ++ name ++ " = " ++ valueToJs value
statementToJs (Assignment target value) = assignmentTargetToJs target ++ " = " ++ valueToJs value
statementToJs (While cond sts) = "while ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (For (init, cond, done) sts) = "for (" ++
  statementToJs init
  ++ "; " ++ valueToJs cond
  ++ "; " ++ statementToJs done
  ++ ") {" ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (IfThenElse cond thens elses) = "if ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs thens) ++ "}"
  ++ flip (maybe "") elses (\sts ->
    " else {" ++ intercalate ";" (map statementToJs sts) ++ "}")
statementToJs (Return value) = "return " ++ valueToJs value

assignmentTargetToJs :: AssignmentTarget -> String
assignmentTargetToJs (AssignVariable var) = var
assignmentTargetToJs (AssignArrayIndex index tgt) = assignmentTargetToJs tgt ++ "[" ++ valueToJs index ++ "]"
assignmentTargetToJs (AssignObjectProperty prop tgt) = assignmentTargetToJs tgt ++ "." ++ prop

-- Externs

typeLiterals :: Pattern Type String
typeLiterals = Pattern $ A.Kleisli match
  where
  match Number = Just "Number"
  match String = Just "String"
  match Boolean = Just "Boolean"
  match (Array ty) = Just $ "[" ++ typeToPs ty ++ "]"
  match (Object row) = Just $ "{ " ++ rowToPs row ++ " }"
  match (TypeVar var) = Just var
  match (TypeConstructor ctor) = Just ctor
  match _ = Nothing

rowToPs :: Row -> String
rowToPs = (\(tys, tail) -> intercalate "; " (map (uncurry nameAndTypeToPs) tys) ++ tailToPs tail) . toList
  where
  nameAndTypeToPs :: String -> Type -> String
  nameAndTypeToPs name ty = name ++ " :: " ++ typeToPs ty
  tailToPs :: Row -> String
  tailToPs REmpty = ""
  tailToPs (RowVar var) = "| " ++ var
  toList :: Row -> ([(String, Type)], Row)
  toList (RCons name ty row) = let (tys, rest) = toList row
                               in ((name, ty):tys, rest)
  toList r = ([], r)

typeApp :: Pattern Type (Type, Type)
typeApp = Pattern $ A.Kleisli match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

function :: Pattern Type ([Type], Type)
function = Pattern $ A.Kleisli match
  where
  match (Function args ret) = Just (args, ret)
  match _ = Nothing

typeToPs :: Type -> String
typeToPs = fromMaybe (error "Incomplete pattern") . pattern matchType
  where
  matchType :: Pattern Type String
  matchType = buildPrettyPrinter operators (typeLiterals <|> fmap parens matchType)
  operators :: OperatorTable Type String
  operators =
    OperatorTable $ [ AssocL typeApp $ \f x -> f ++ " " ++ x
                    , Split function $ \args ret -> "(" ++ intercalate "," (map typeToPs args) ++ ") -> " ++ typeToPs ret ]

externToJs :: (String, Type) -> String
externToJs (name, ty) = "extern " ++ name ++ " :: " ++ typeToPs ty
